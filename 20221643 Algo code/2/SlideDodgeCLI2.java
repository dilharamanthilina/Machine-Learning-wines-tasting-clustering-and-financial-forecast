// ID: 20221643
// Name : Dilhara Manthilina

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class SlideDodgeCLI2 {
    private static int WIDTH = 0;
    private static int HEIGHT = 0;
    private static char[][] grid;
    private static int playerX;
    private static int playerY;
    private static int finalX;
    private static int finalY;
    private static String choice = "";
    private static final char PLAYER_CHAR = '@';
    private static final char OBSTACLE_CHAR = '0';
    private static final char START_SYMBOL = 'S';
    private static final char FINAL_STAGE = 'F';
    private static Scanner scanner;

    public static void main(String[] args) {
        scanner = new Scanner(System.in);
        System.out.println("Welcome to Maze game");

        try {
            // Read the maze map from the file
            File file = new File("D:\\2\\a.txt");
            Scanner fileScanner = new Scanner(file);

            int height = 0;
            int width = -1;

            // Determine height and width of the maze
            while (fileScanner.hasNextLine()) {
                HEIGHT++;
                String line = fileScanner.nextLine();
                if (line.length() > 0) {
                    WIDTH = line.length();
                }
            }

            System.out.println("Height: " + HEIGHT + " Width: " + WIDTH);

            // Initialize the grid
            grid = new char[HEIGHT][WIDTH];
            fileScanner = new Scanner(file);

            // Populate the grid
            int y = 0;
            while (fileScanner.hasNextLine()) {
                String line = fileScanner.nextLine();
                for (int x = 0; x < WIDTH; x++) {
                    grid[y][x] = line.charAt(x);
                    if (grid[y][x] == START_SYMBOL) {
                        playerX = y;
                        playerY = x;
                    }
                    if (grid[y][x] == FINAL_STAGE) {
                        finalX = y;
                        finalY = x;
                    }
                }
                y++;
            }

            fileScanner.close();
            System.out.println("Start: " + playerX + " " + playerY);
            System.out.println("Final: " + finalX + " " + finalY);

            printGrid();

            // Game logic can continue here

        } catch (FileNotFoundException e) {
            System.out.println("File not found.");
        }
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
        int newPlayerY = playerX + deltaY;
        int newPlayerX = playerY + deltaX;

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
            grid[playerX][playerY] = '.';
            playerX = newPlayerY;
            playerY = newPlayerX;
            grid[playerX][playerY] = PLAYER_CHAR;
            printGrid();

        } else {
            System.out.println("Invalid move! Try again.");
        }
    }

    private static boolean isValidMove(int newY, int newX) {
        return newY >= 0 && newY < HEIGHT && newX >= 0 && newX < WIDTH && grid[newY][newX] != OBSTACLE_CHAR;
    }
}
