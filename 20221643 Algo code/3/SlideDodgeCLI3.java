// ID: 20221643
// Name : Dilhara Manthilina

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayDeque;
import java.util.Queue;
import java.util.Scanner;

public class SlideDodgeCLI3 {
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

            // Find shortest path
            findShortestPath();

            // Game logic can continue here

        } catch (FileNotFoundException e) {
            System.out.println("File not found.");
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

    private static void findShortestPath() {
        boolean[][] visited = new boolean[HEIGHT][WIDTH];
        Queue<Point> queue = new ArrayDeque<>();
        queue.add(new Point(playerX, playerY, null));

        while (!queue.isEmpty()) {
            Point current = queue.poll();
            int x = current.x;
            int y = current.y;

            if (x == finalX && y == finalY) {
                // Found the final stage, print the path
                printPath(current);
                return;
            }

            // Check neighboring cells
            for (int[] dir : DIRECTIONS) {
                int newX = x + dir[0];
                int newY = y + dir[1];

                if (isValidMove(newX, newY) && !visited[newX][newY]) {
                    visited[newX][newY] = true;
                    queue.add(new Point(newX, newY, current));
                }
            }
        }

        System.out.println("No path found.");
    }

    private static void printPath(Point end) {
        Point current = end;
        int step = 1;
        while (current != null) {
            System.out.println(step + ". Move to (" + (current.x + 1) + ", " + (current.y + 1) + ")");
            current = current.prev;
            step++;
        }
        System.out.println("Done!");
    }

    private static boolean isValidMove(int x, int y) {
        return x >= 0 && x < HEIGHT && y >= 0 && y < WIDTH && grid[x][y] != OBSTACLE_CHAR;
    }

    private static class Point {
        int x;
        int y;
        Point prev;

        Point(int x, int y, Point prev) {
            this.x = x;
            this.y = y;
            this.prev = prev;
        }
    }

    private static final int[][] DIRECTIONS = { { -1, 0 }, { 1, 0 }, { 0, -1 }, { 0, 1 } };
}
