// ID: 20221643
// Name : Dilhara Manthilina

import java.util.Scanner;

public class SlideDodgeCLI {
    private static final int WIDTH = 10;
    private static final int HEIGHT = 10;
    private static final char PLAYER_CHAR = '@';
    private static final char OBSTACLE_CHAR = '0';

    private static char[][] grid;
    private static int playerX;
    private static int playerY;
    private static int score;
    private static Scanner scanner;
    private static String choice = "";

    public static void main(String[] args) {
        scanner = new Scanner(System.in);

        System.out.println("Welcome to Maze game");

        grid = new char[][] {
                { '.', '.', '.', '.', '.', '0', '.', '.', '.', 'S' },
                { '.', '.', '.', '.', '0', '.', '.', '.', '.', '.' },
                { '0', '.', '.', '.', '.', '.', '0', '.', '.', '0' },
                { '.', '.', '.', '0', '.', '.', '.', '.', '0', '.' },
                { '.', 'F', '.', '.', '.', '.', '.', '0', '0', '.' },
                { '.', '0', '.', '.', '.', '.', '.', '.', '.', '.' },
                { '.', '.', '.', '.', '.', '.', '.', '0', '.', '.' },
                { '.', '0', '.', '0', '.', '.', '0', '.', '.', '0' },
                { '0', '.', '.', '.', '.', '.', '.', '.', '.', '.' },
                { '.', '0', '0', '.', '.', '.', '.', '.', '0', '.' }
        };

        playerX = 9;
        playerY = 0;

        printGrid();

        while (!choice.equals("Q")) {
            System.out.println(
                    "\nPress W to move up \nPress S to move down \nPress A to move left \nPress D to move right \nPress Q to quit \n ");

            System.out.print("Enter your choice: ");
            choice = scanner.nextLine().toUpperCase();

            switch (choice) {
                case "W":
                    movePlayer(-1, 0);
                    break;
                case "S":
                    movePlayer(1, 0);
                    break;
                case "A":
                    movePlayer(0, -1);
                    break;
                case "D":
                    movePlayer(0, 1);
                    break;
            }
        }
        scanner.close();
    }

    private static void printGrid() {
        for (char[] row : grid) {
            for (char cell : row) {
                System.out.print(cell + " ");
            }
            System.out.println();
        }
    }

    private static void movePlayer(int deltaY, int deltaX) {
        int newPlayerY = playerY + deltaY;
        int newPlayerX = playerX + deltaX;

        if (isValidMove(newPlayerY, newPlayerX)) {
            while (isValidMove(newPlayerY + deltaY, newPlayerX + deltaX) && grid[newPlayerY][newPlayerX] != 'F') {
                newPlayerY += deltaY;
                newPlayerX += deltaX;
                if (grid[newPlayerY][newPlayerX] == OBSTACLE_CHAR) {
                    break;
                }
            }

            if (grid[newPlayerY][newPlayerX] == 'F') {
                System.out.println("Congratulations! You won.");
                choice = "Q";
            }
            grid[playerY][playerX] = '.';
            playerY = newPlayerY;
            playerX = newPlayerX;
            grid[playerY][playerX] = PLAYER_CHAR;
            printGrid();

        } else {
            System.out.println("Invalid move! Try again.");
        }
    }

    private static boolean isValidMove(int newY, int newX) {
        return newY >= 0 && newY < HEIGHT && newX >= 0 && newX < WIDTH && grid[newY][newX] != OBSTACLE_CHAR;
    }
}
