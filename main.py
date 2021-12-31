import sys

from .board import Board

DEBUG = len(sys.argv) >= 2 and sys.argv[1] == '--debug'


def debug(*values):
    if DEBUG:
        print(*values)


class main():
    def next_board(self, action, board):
        pos_from, pos_to, nums = action
        debug('action: ', [pos_from + 1, pos_to + 1, nums])
        if board[pos_from] == 0:
            return False

        if (pos_from in [0, 1] and pos_to in [2, 3]) or (pos_from in [2, 3] and pos_to in [0, 1]):
            if (board[pos_from] == 0 or board[pos_to] == 0):
                return False
            board[pos_to] += board[pos_from]
            board[pos_to] %= 5
            return True
        elif ((pos_from in [0, 1] and pos_to in [0, 1]) or (pos_from in [2, 3] and pos_to in [2, 3])) and pos_from != pos_to and nums > 0 and board[pos_from] >= nums:
            tmp_from = board[pos_from]
            tmp_to = board[pos_to]

            tmp_from -= nums
            tmp_to += nums
            tmp_to %= 5

            if tmp_from == board[pos_to] and tmp_to == board[pos_from]:
                return False

            board[pos_from] = tmp_from
            board[pos_to] = tmp_to
            return True
        return False

    def show_board(self, board):
        print('あなた: {0[0]} {0[1]} | AI: {0[2]} {0[3]}'.format(
            board), flush=True)

    def __init__(self):
        self.board = [1, 1, 1, 1]
        self.my_turn = True

        self.state = "プレイ中"
        self.show_board(self.board)
        while self.state == "プレイ中":
            if self.my_turn:
                while True:
                    pos_from = int(input('あなたの番です >> 開始位置 >> ')) - 1
                    pos_to = int(input('あなたの番です >> 対象位置 >> ')) - 1
                    nums = 0
                    if pos_to in [0, 1] and pos_from in [0, 1] and pos_from != pos_to:
                        nums = int(input('あなたの番です >> 移動する数 >> '))
                    if self.next_board([pos_from, pos_to, nums], self.board):
                        self.state = self.check_state(self.board)
                        self.my_turn = not(self.my_turn)
                        break
                    else:
                        print('位置に誤りがあります')
            else:
                action = self.minimax(self.board, 'プレイ中', 0)
                print(action)
                print('AIアクション: ', action[0] + 1, action[1] + 1, action[2])
                self.next_board(action, self.board)
                self.state = self.check_state(self.board)
                self.my_turn = not(self.my_turn)

            self.show_board(self.board)

        print(self.state)
        if self.state != 'プレイ中':
            sys.exit()

    def check_state(self, board):
        if board[0] == 0 and board[1] == 0:
            return 'あなたの負けです'
        elif board[2] == 0 and board[3] == 0:
            return 'あなたの勝ちです'
        return "プレイ中"

    def minimax(self, board, state, depth):
        debug('*' * depth + 'minimax: ' + state)
        if state != "プレイ中":
            return self.evaluate(board, state, depth)
        if depth > 8:
            v = self.evaluate(board, state, depth)
            if DEBUG:
                self.show_board(board)
            debug(v)
            return v

        best_value = 0
        value = 100 if self.my_turn else -100

        my_from = range(0, 2) if self.my_turn else range(2, 4)
        for pos_from in my_from:
            for pos_to in range(4):
                nums_range = [0]
                if (pos_from in [0, 1] and pos_to in [0, 1]) or \
                        (pos_from in [2, 3] and pos_to in [2, 3]):
                    nums_range = range(board[pos_from])
                for nums in nums_range:
                    new_board = board.copy()
                    if self.next_board([pos_from, pos_to, nums + 1], new_board):
                        if DEBUG:
                            self.show_board(board)
                        debug('action: ', [pos_from + 1, pos_to + 1, 1])
                        if DEBUG:
                            self.show_board(new_board)
                        state = self.check_state(new_board)
                        debug('state: ' + state)
                        self.my_turn = not(self.my_turn)

                        child_value = self.minimax(new_board, state, depth + 1)
                        debug([child_value, depth])
                        if not self.my_turn:
                            if child_value < value:
                                value = child_value
                                best_value = [pos_from, pos_to, nums + 1]
                        else:
                            if child_value > value:
                                value = child_value
                                best_value = [pos_from, pos_to, nums + 1]

                        self.my_turn = not(self.my_turn)
                        if depth == 0:
                            print(
                                "アクション：", [pos_from + 1, pos_to + 1, nums + 1], "評価点：", child_value, "）")

        if depth == 0:
            return best_value
        else:
            return value

    def same_hand(self, hand, pattern):
        return (hand[0] == pattern[0] and hand[1] == pattern[1]) or \
            (hand[0] == pattern[1] and hand[1] == pattern[0])

    def evaluate(self, board, state, depth):
        if state == "あなたの負けです":
            return 100 - depth
        elif state == "あなたの勝ちです":
            return depth - 100
        else:
            my_hand = [board[0], board[1]] if not self.my_turn else [
                board[2], board[3]]
            op_hand = [board[2], board[3]] if not self.my_turn else [
                board[0], board[1]]

            if self.same_hand(my_hand, [0, 1]):
                return depth - 50
            elif self.same_hand(op_hand, [0, 1]):
                return 50 - depth
            elif self.same_hand(my_hand, [1, 2]) and self.same_hand(op_hand, [3, 2]):
                return 75 - depth
            elif self.same_hand(my_hand, [3, 2]) and self.same_hand(op_hand, [1, 2]):
                # self.show_board(board)
                return depth - 75
            elif self.same_hand(my_hand, [2, 2]) and self.same_hand(op_hand, [1, 2]):
                return 50 - depth
            elif self.same_hand(my_hand, [1, 2]) and self.same_hand(op_hand, [2, 2]):
                return 50 - depth
            return 0


if __name__ == '__main__':
    main()
