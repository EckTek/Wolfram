module Utils where

-- Haskell import
import System.IO
import System.Exit

defaultOptions = ["0", "0", "-1", "80", "0"]

rulesets = ["        ", "      * ", "      * ", "      **"
           ,"     *  ", "    * * ", "     ** ", "     ***"
           ,"    *   ", "   *  * ", "    * * ", "    * **"
           ,"    **  ", "   ** * ", "    *** ", "    ****" --15
           ,"   *    ", "  *   * ", "   *  * ", "   *  **"
           ,"   * *  ", "  * * * ", "   * ** ", "   * ***"
           ,"   **   ", "  **  * ", "   ** * ", "   ** **"
           ,"   ***  ", "  *** * ", "   **** ", "   *****" --31
           ,"  *     ", "  *    *", "  *   * ", "  *   **"
           ,"  *  *  ", "  *  * *", "  *  ** ", "  *  ***"
           ,"  * *   ", "  * *  *", "  * * * ", "  * * **"
           ,"  * **  ", "  * ** *", "  * *** ", "  * ****" --47
           ,"  **    ", "  **   *", "  **  * ", "  **  **"
           ,"  ** *  ", "  ** * *", "  ** ** ", "  ** ***"
           ,"  ***   ", "  ***  *", "  *** * ", "  *** **"
           ,"  ****  ", "  **** *", "  ***** ", "  ******" --63
           ," *      ", " *     *", " *    * ", " *    **"
           ," *   *  ", " *   * *", " *   ** ", " *   ***"
           ," *  *   ", " *  *  *", " *  * * ", " *  * **"
           ," *  **  ", " *  ** *", " *  *** ", " *  ****" --79
           ," * *    ", " * *   *", " * *  * ", " * *  **"
           ," * * *  ", " * * * *", " * * ** ", " * * ***"
           ," * **   ", " * **  *", " * ** * ", " * ** **"
           ," * ***  ", " * *** *", " * **** ", " * *****" --95
           ," **     ", " **    *", " **   * ", " **   **"
           ," **  *  ", " **  * *", " **  ** ", " **  ***"
           ," ** *   ", " ** *  *", " ** * * ", " ** * **"
           ," ** **  ", " ** ** *", " ** *** ", " ** ****" --111
           ," ***    ", " ***   *", " ***  * ", " ***  **"
           ," *** *  ", " *** * *", " *** ** ", " *** ***"
           ," ****   ", " ****  *", " **** * ", " **** **"
           ," *****  ", " ***** *", " ****** ", " *******" --127
           ,"*       ", "*     * ", "*     * ", "*     **"
           ,"*    *  ", "*   * * ", "*    ** ", "*    ***"
           ,"*   *   ", "*  *  * ", "*   * * ", "*   * **"
           ,"*   **  ", "*  ** * ", "*   *** ", "*   ****" --143
           ,"*  *    ", "* *   * ", "*  *  * ", "*  *  **"
           ,"*  * *  ", "* * * * ", "*  * ** ", "*  * ***"
           ,"*  **   ", "* **  * ", "*  ** * ", "*  ** **"
           ,"*  ***  ", "* *** * ", "*  **** ", "*  *****" --159
           ,"* *     ", "* *    *", "* *   * ", "* *   **"
           ,"* *  *  ", "* *  * *", "* *  ** ", "* *  ***"
           ,"* * *   ", "* * *  *", "* * * * ", "* * * **"
           ,"* * **  ", "* * ** *", "* * *** ", "* * ****" --175
           ,"* **    ", "* **   *", "* **  * ", "* **  **"
           ,"* ** *  ", "* ** * *", "* ** ** ", "* ** ***"
           ,"* ***   ", "* ***  *", "* *** * ", "* *** **"
           ,"* ****  ", "* **** *", "* ***** ", "* ******" --191
           ,"**      ", "**     *", "**    * ", "**    **"
           ,"**   *  ", "**   * *", "**   ** ", "**   ***"
           ,"**  *   ", "**  *  *", "**  * * ", "**  * **"
           ,"**  **  ", "**  ** *", "**  *** ", "**  ****" --207
           ,"** *    ", "** *   *", "** *  * ", "** *  **"
           ,"** * *  ", "** * * *", "** * ** ", "** * ***"
           ,"** **   ", "** **  *", "** ** * ", "** ** **"
           ,"** ***  ", "** *** *", "** **** ", "** *****" --223
           ,"***     ", "***    *", "***   * ", "***   **"
           ,"***  *  ", "***  * *", "***  ** ", "***  ***"
           ,"*** *   ", "*** *  *", "*** * * ", "*** * **"
           ,"*** **  ", "*** ** *", "*** *** ", "*** ****" --239
           ,"****    ", "****   *", "****  * ", "****  **"
           ,"**** *  ", "**** * *", "**** ** ", "**** ***"
           ,"*****   ", "*****  *", "***** * ", "***** **"
           ,"******  ", "****** *", "******* ", "********"] --255


helpMsg = "Usage: ./wolfram [OPTION] [VALUE]...\n\n \
           \--rule   : the ruleset to use (no default value, mandatory)\n \
           \--start  : the generation number at which to start the display. The default value is 0.\n \
           \--lines  : the number of lines to display. When homited, the program never stops.\n \
           \--window : the number of cells to display on each line (line width).\n \
           \           If even, the central cell is display in the next cell on the right. The default value is 80.\n \
           \--move   : a translation to apply on the window. If negative, the window is translated to the left.\n \
           \           If positive, it’s translated to the right."

          
exitWithError :: String -> IO a
exitWithError err = hPutStrLn stderr (err ++ "\nRerun with -h or --help.") >> exitWith (ExitFailure 84)

printHelp :: String -> IO ()
printHelp msg = hPutStrLn stdout msg >> exitSuccess

-- Alias
exit = exitWithError
help = printHelp
