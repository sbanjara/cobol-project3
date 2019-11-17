     
      ********************* IDENTIFICATION-DIVISION ****************** 

       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PROGRAM3.
       AUTHOR.      SBANJARA.

      ****************************************************************
      *
      *  PRODUCE A SUMMARY REPORT FROM THE INPUT FILE SHOWING THE
      *  LIST OF CANDY INVENTORY BROKEN INTO THE 5 DIFFERENT
      *  WAREHOUSES AND THE 4 DIFFERENT VENDORS.  CHECK FOR INCORRECT
      *  DATA.
      *
      *  INPUT: WAREHOUSE-ID, VENDOR-ID, CANDY-ID, CANDY-DATA:
      *         CANDY-NAME, CANDY-BOX-SIZE, CANDY-TYPE, QUANTITY-STOCK,
      *         PURCH-PRICE
      *
      *  OUTPUT: 
      *          HEADER-LINES:
      *          REPORT-HEADERS(2) INCLUDING DATE 
      *          WAREHOUSE-HEADER INCLUDING NAME
      *          VENDOR-HEADER INCLUDING NAME EXPANDED AND VALIDATED, 
      *          CANDY-HEADER INCLUDING CANDY CODE
      *          COLUMN-HEADER
      *
      *          DETAIL-LINE:
      *          CANDY-NAME 
      *          CANDY-SIZE EXPANDED
      *          CANDY-TYPE
      *          QUANTITY-IN-STOCK VALIDATED
      *          CANDY-IN-STOCK-TOTAL-COST - QUANTITY * PURCHASE PRICE
      *
      *          GROUP-LINES:
      *          CANDY-TOTAL INCLUDING CANDY NAME
      *          VENDOR-TOTAL INCLUDING VENDOR NAME
      *          WAREHOUSE-TOTAL INCUDING WARENOUSE NAME
      *          GRAND-TOTAL
      *
      *  CALCULATIONS: 
      *          CANDY-COST = QUANTITY * PURCHASE-PRICE
      *          CANDY-TOTAL = CANDY-TOTAL + CANDY-COST
      *          VENDOR-TOTAL = VENDOR-TOTAL + CANDY-TOTAL
      *          WAREHOUSE-TOTAL = WAREHOUSE-TOTAL + VENDOR-TOTAL
      *          GRAND-TOTAL = GRAND-TOTAL + WAREHOUSE-TOTAL
      *
      ****************************************************************

      ********************** ENVIRONMENT-DIVISION ********************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LENEVO-PC.
       OBJECT-COMPUTER. LENEVO-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE
               ASSIGN TO "PR3FA19.TXT"
			   ORGANIZATION IS LINE SEQUENTIAL.
	  		   
           SELECT REPORT-FILE
               ASSIGN TO "REPORT.TXT".

      *********************** DATA-DIVISION **************************

       DATA DIVISION.
       FILE SECTION.

       FD  INVENTORY-FILE
           RECORD CONTAINS 143 CHARACTERS.

       01  INVENTORY-RECORD.
           05  WAREHOUSE-ID      PIC X(4).
           05  VENDOR-ID         PIC X(1).
           05  CANDY-ID          PIC X(3).
           05  CANDY-DATA OCCURS 5 TIMES.
               10  CANDY-NAME    PIC X(15).
               10  BOX-SIZE      PIC A.
               10  CANDY-TYPE    PIC AA.
               10  NUM-CASES     PIC S9(4).
               10  PRICE         PIC S999V99.

       FD  REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.

       01  REPORT-RECORD        PIC X(80).

      ******************** WORKING STORAGE ***************************

       WORKING-STORAGE SECTION.

       01  FLAG-AND-SWITCHES.
           05  EOF-FILE            PIC X       VALUE ' '.  
		       88  NO-MORE-DATA                VALUE 'N'.
           05  FIRST-RECORD        PIC X(3)    VALUE 'YES'.

       01  WS-DATE.
           05  WS-YEAR             PIC 9999.
           05  WS-MONTH            PIC 99.
           05  WS-DAY              PIC 99.
          
       01  REPORT-FIELDS.
           05  PROPER-SPACING      PIC 9        VALUE 1.
           05  SUB                 PIC 99       VALUE 1.
           05  PAGE-NUM            PIC 99       VALUE 0.
    
       01  DETAIL-FIELDS.
           05  DF-WAREHOUSE-HOLD   PIC X(4).
           05  DF-VENDOR-HOLD      PIC X.
           05  DF-CANDY-HOLD       PIC X(3).
           05  DF-VENDOR-NAME      PIC X(18)    VALUE SPACES.
		   05  DF-VENDOR-NAME-TEMP PIC X(18)    VALUE SPACES.
           05  DF-CANDY-SIZE       PIC X(15)    VALUE SPACES.
           05  DF-CANDY-NAME       PIC X(15)    VALUE SPACES.
           05  DF-CANDY-NAME-TEMP  PIC X(15).
           05  DF-NUM-OF-CASES     PIC S9(4)    VALUE 0.
           05  DF-PURCHASE-PRICE   PIC S9(6)V99 VALUE 0.
           05  DF-TOTAL-COST       PIC 9(6)V99  VALUE 0.
           05  DF-CANDY-TOTAL      PIC 9(7)V99  VALUE 0.
           05  DF-VENDOR-TOTAL     PIC 9(7)V99  VALUE 0.
           05  DF-WAREHOUSE-TOTAL  PIC 9(7)V99  VALUE 0.
           05  DF-GRAND-TOTAL      PIC 9(7)V99  VALUE 0.

      *********************** OUTPUT AREA ****************************

       01  HEADING-ONE.
           05                   PIC X(31)   VALUE SPACES.
           05                   PIC X(49)   VALUE 'GLENCOE CANDY, LTD'.

       01  HEADING-TWO.
           05                  PIC X(7)      VALUE SPACES.
           05  H2-DATE.
               10  H2-DAY      PIC 99.
               10              PIC X         VALUE '/'.
               10  H2-MONTH    PIC 99.
               10              PIC X         VALUE '/'.
               10  H2-YEAR     PIC 9(4).
           05                  PIC X(15)     VALUE SPACES.
           05                  PIC X(25)     VALUE 'INVENTORY REPORT'.
           05                  PIC X(6)      VALUE 'PAGE: '.
           05  H2-PAGE-NUM     PIC 99        VALUE 0.

       01  WAREHOUSE-HEADING.
           05                  PIC X(13)      VALUE '  WAREHOUSE: '.
           05  WAREHOUSE-CODE  PIC X(4).
           05                  PIC X(63)      VALUE SPACES.

       01  VENDOR-HEADING.
           05                  PIC X(13)      VALUE '     VENDOR: '.
           05  VENDOR-NAME     PIC X(18).
           05                  PIC X(49)      VALUE SPACES.

       01  CANDY-HEADING.
           05                  PIC X(13)      VALUE '      CANDY: '.
           05  CH-CANDY-CODE   PIC X(3).
           05                  PIC X(64)      VALUE SPACES.

       01  HEADING-THREE.
           05                  PIC X(10)      VALUE SPACES.
           05                  PIC X(14)      VALUE 'NAME'.
           05                  PIC X(12)      VALUE 'SIZE'.
           05                  PIC X(7)       VALUE 'TYPE'.
           05                  PIC X(12)      VALUE 'IN STOCK'.
           05                  PIC X(25)      VALUE 'TOTAL COST'.

       01  DETAIL-LINE.
           05                  PIC X(5)       VALUE SPACES.
           05  DL-NAME         PIC X(17).
           05  DL-SIZE         PIC X(15)      VALUE SPACES.
           05  DL-TYPE         PIC X(8).
           05  DL-IN-STOCK     PIC Z999.
           05                  PIC X(6)       VALUE SPACES.
           05  DL-TOTAL-COST   PIC $$$,$$$.99.
           05                  PIC X(15)      VALUE SPACES.

       01  CANDY-TOTAL-LINE.
           05                  PIC X(19)      VALUE SPACES.
           05                  PIC X(13)      VALUE 'TOTAL CANDY: '.
           05  CTL-NAME        PIC X(21).
           05  CTL-TOTAL-COST  PIC $,$$$,$$$.99.
        
       01  VENDOR-TOTAL-LINE.
           05                  PIC X(14)     VALUE SPACES.
           05                  PIC X(18)     VALUE 'TOTAL FOR VENDOR: '.
           05  VTL-NAME        PIC X(21).
           05  VTL-TOTAL-COST  PIC $,$$$,$$$.99.

       01  WAREHOUSE-TOTAL-LINE.
           05                  PIC X(12)  VALUE SPACES.
           05                  PIC X(21)  VALUE 'TOTAL FOR WAREHOUSE:'.
           05  WTL-NAME        PIC X(20).
           05  WTL-TOTAL-COST  PIC $$,$$$,$$$.99.

       01  GRAND-TOTAL-LINE.
           05                  PIC X(19)   VALUE SPACES.
           05                  PIC X(32)   VALUE 'GRAND TOTAL:'.
           05  GTL-TOTAL-COST  PIC $$$,$$$,$$$.99.

      ************************** PROCEDURE-DIVISION ******************
      
       PROCEDURE DIVISION.

       50-CONTROL-MODULE.

           PERFORM 100-HOUSEKEEPING-ROUTINE
           PERFORM 200-READ-THE-FILE
           PERFORM 800-EOF-ROUTINE
		   PERFORM 900-PRINT-FINAL-TOTAL
           PERFORM 1000-FINAL-ROUTINE
		   
           .

       100-HOUSEKEEPING-ROUTINE.

           OPEN INPUT  INVENTORY-FILE
                OUTPUT REPORT-FILE

           ACCEPT WS-DATE FROM DATE YYYYMMDD
           MOVE WS-DAY   TO H2-DAY
           MOVE WS-MONTH TO H2-MONTH
           MOVE WS-YEAR  TO H2-YEAR
		   
           .

       150-WRITE-LINE.
           
           WRITE REPORT-RECORD AFTER 
                 ADVANCING PROPER-SPACING
				 
           .
    
       200-READ-THE-FILE.
           
           PERFORM UNTIL NO-MORE-DATA
               READ INVENTORY-FILE 
                   AT END  
                       MOVE 'N' TO EOF-FILE
                   NOT AT END
                       PERFORM 500-PROCESS-INVENTORY-RECORD
               END-READ
           END-PERFORM 
		   
           .
        
       250-HEADER-ROUTINE.
		   
		   ADD 1 TO PAGE-NUM
		   WRITE REPORT-RECORD FROM HEADING-ONE
                AFTER ADVANCING PAGE

           MOVE PAGE-NUM TO H2-PAGE-NUM
           MOVE HEADING-TWO TO REPORT-RECORD
		   MOVE 1 TO PROPER-SPACING
		   PERFORM 150-WRITE-LINE
           MOVE 2 TO PROPER-SPACING
		   
           .

       300-PRINT-WAREHOUSE-HEADER.
       
           MOVE WAREHOUSE-ID TO WAREHOUSE-CODE
           MOVE WAREHOUSE-HEADING TO REPORT-RECORD
           PERFORM 150-WRITE-LINE
		   
           .

       350-PRINT-VENDOR-HEADER.
           
           EVALUATE TRUE

               WHEN VENDOR-ID = 'A'
                   MOVE 'ATOMIC SWEETS' TO DF-VENDOR-NAME
           
               WHEN VENDOR-ID = 'B'
                   MOVE 'BOOZIE SWEETS' TO DF-VENDOR-NAME
                   
               WHEN VENDOR-ID = 'N'
                   MOVE 'NELLIES SWEET SHOP' TO DF-VENDOR-NAME
                  
               WHEN VENDOR-ID = 'T'
                   MOVE 'TIGER TREATS' TO DF-VENDOR-NAME
                   
               WHEN OTHER
                   STRING
                       'INVALID' DELIMITED BY SIZE
                       '-'       DELIMITED BY SIZE
                       VENDOR-ID DELIMITED BY SIZE
                       INTO DF-VENDOR-NAME
                   END-STRING		   
				  
           END-EVALUATE

           MOVE DF-VENDOR-NAME  TO VENDOR-NAME
		   MOVE DF-VENDOR-NAME TO DF-VENDOR-NAME-TEMP
           MOVE VENDOR-HEADING TO REPORT-RECORD
           PERFORM 150-WRITE-LINE
		   MOVE SPACES TO DF-VENDOR-NAME
		   
           .

       400-PRINT-CANDY-HEADER.

           MOVE DF-CANDY-HOLD TO CH-CANDY-CODE
           MOVE CANDY-HEADING TO REPORT-RECORD
           PERFORM 150-WRITE-LINE
		   
           .

       450-PRINT-HEADING-THREE.

           MOVE HEADING-THREE TO REPORT-RECORD
           PERFORM 150-WRITE-LINE

           .

       500-PROCESS-INVENTORY-RECORD.

           EVALUATE TRUE  
              
               WHEN FIRST-RECORD = 'YES'
                   MOVE 'NO' TO FIRST-RECORD
                   MOVE WAREHOUSE-ID TO DF-WAREHOUSE-HOLD
                   MOVE VENDOR-ID    TO DF-VENDOR-HOLD
                   MOVE CANDY-ID     TO DF-CANDY-HOLD
                   PERFORM 250-HEADER-ROUTINE
                   PERFORM 300-PRINT-WAREHOUSE-HEADER
                   PERFORM 350-PRINT-VENDOR-HEADER
                   PERFORM 400-PRINT-CANDY-HEADER
                   PERFORM 450-PRINT-HEADING-THREE

               WHEN WAREHOUSE-ID NOT = DF-WAREHOUSE-HOLD
                   PERFORM 600-WAREHOUSE-BREAK
                   MOVE 3 TO PROPER-SPACING
                   PERFORM 250-HEADER-ROUTINE
                   PERFORM 300-PRINT-WAREHOUSE-HEADER
                   PERFORM 350-PRINT-VENDOR-HEADER
                   PERFORM 400-PRINT-CANDY-HEADER
                   PERFORM 450-PRINT-HEADING-THREE

               WHEN VENDOR-ID NOT = DF-VENDOR-HOLD
                   PERFORM 650-VENDOR-BREAK
                   MOVE 3 TO PROPER-SPACING
                   PERFORM 350-PRINT-VENDOR-HEADER
				   MOVE 2 TO PROPER-SPACING
                   PERFORM 400-PRINT-CANDY-HEADER
                   PERFORM 450-PRINT-HEADING-THREE

               WHEN CANDY-ID NOT = DF-CANDY-HOLD
                   PERFORM 700-CANDY-BREAK
				   MOVE 3 TO PROPER-SPACING
                   PERFORM 400-PRINT-CANDY-HEADER 
                   MOVE 2 TO PROPER-SPACING
                   PERFORM 450-PRINT-HEADING-THREE
                   MOVE 2 TO PROPER-SPACING

           END-EVALUATE
           
           
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
            
               IF CANDY-NAME(SUB) = DF-CANDY-NAME
                   MOVE SPACES TO DL-NAME
               ELSE
                   MOVE CANDY-NAME(SUB) TO DL-NAME
                   MOVE CANDY-NAME(SUB) TO DF-CANDY-NAME
               END-IF
			   
			   MOVE CANDY-NAME(1) TO DF-CANDY-NAME-TEMP

               EVALUATE TRUE

                   WHEN BOX-SIZE(SUB) = 'L'
                       MOVE 'LARGE' TO DF-CANDY-SIZE
                   WHEN BOX-SIZE(SUB) = 'M'
                       MOVE 'MEDIUM' TO DF-CANDY-SIZE
                   WHEN BOX-SIZE(SUB) = 'S'
                       MOVE 'SMALL' TO DF-CANDY-SIZE
                   WHEN BOX-SIZE(SUB) = 'G'
                       MOVE 'GIFT'  TO DF-CANDY-SIZE
                   WHEN BOX-SIZE(SUB) = 'X'
                       MOVE 'SAMPLE' TO DF-CANDY-SIZE
                   WHEN BOX-SIZE(SUB) = ' '
                       MOVE SPACES TO DF-CANDY-SIZE
                   WHEN OTHER
				       STRING				   
                           'BAD' DELIMITED BY SIZE
                           '-'   DELIMITED BY SIZE
                           BOX-SIZE(SUB) DELIMITED BY SIZE
                           INTO DF-CANDY-SIZE
                       END-STRING

               END-EVALUATE

               MOVE DF-CANDY-SIZE TO DL-SIZE
               MOVE SPACES TO DF-CANDY-SIZE

               MOVE CANDY-TYPE(SUB) TO DL-TYPE

               IF NUM-CASES(SUB) IS NUMERIC
                   MOVE NUM-CASES(SUB) TO DF-NUM-OF-CASES
               ELSE 
                   MOVE 0 TO DF-NUM-OF-CASES
               END-IF
			   
			   MOVE DF-NUM-OF-CASES TO DL-IN-STOCK

               IF PRICE(SUB) IS NUMERIC
                   MULTIPLY PRICE(SUB) BY DF-NUM-OF-CASES
                       GIVING DF-PURCHASE-PRICE
               ELSE
                   MOVE 0 TO DF-PURCHASE-PRICE
               END-IF
			   
			   MOVE DF-PURCHASE-PRICE TO DL-TOTAL-COST

               ADD DF-PURCHASE-PRICE TO DF-CANDY-TOTAL
               ADD DF-PURCHASE-PRICE TO DF-VENDOR-TOTAL
               ADD DF-PURCHASE-PRICE TO DF-WAREHOUSE-TOTAL
               ADD DF-PURCHASE-PRICE TO DF-GRAND-TOTAL
			   MOVE ZEROS TO DF-NUM-OF-CASES
			   MOVE ZEROS TO DF-TOTAL-COST
			   MOVE ZEROS TO DF-PURCHASE-PRICE

               MOVE DETAIL-LINE TO REPORT-RECORD
               PERFORM 150-WRITE-LINE
               MOVE 1 TO PROPER-SPACING
    
           END-PERFORM
		   
           .

       600-WAREHOUSE-BREAK.
           
           PERFORM 650-VENDOR-BREAK
           MOVE DF-WAREHOUSE-HOLD    TO WTL-NAME
           MOVE DF-WAREHOUSE-TOTAL   TO WTL-TOTAL-COST
           MOVE WAREHOUSE-TOTAL-LINE TO REPORT-RECORD
           MOVE 2 TO PROPER-SPACING
           PERFORM 150-WRITE-LINE
           MOVE 1 TO PROPER-SPACING
           MOVE ZEROS TO WTL-TOTAL-COST
           MOVE ZEROS TO DF-WAREHOUSE-TOTAL
           MOVE WAREHOUSE-ID TO DF-WAREHOUSE-HOLD
		   
           .

       650-VENDOR-BREAK.
           
           PERFORM 700-CANDY-BREAK
           MOVE DF-VENDOR-NAME-TEMP  TO VTL-NAME
           MOVE DF-VENDOR-TOTAL TO VTL-TOTAL-COST
           MOVE VENDOR-TOTAL-LINE TO REPORT-RECORD
           MOVE 2 TO PROPER-SPACING
           PERFORM 150-WRITE-LINE
           MOVE 1 TO PROPER-SPACING
           MOVE ZEROS TO VTL-TOTAL-COST
           MOVE ZEROS TO DF-VENDOR-TOTAL
           MOVE VENDOR-ID TO DF-VENDOR-HOLD
		   
           .

       700-CANDY-BREAK.
	   
           MOVE DF-CANDY-NAME-TEMP  TO CTL-NAME
           MOVE DF-CANDY-TOTAL TO CTL-TOTAL-COST
           MOVE CANDY-TOTAL-LINE TO REPORT-RECORD
           MOVE 2 TO PROPER-SPACING
           PERFORM 150-WRITE-LINE
           MOVE 1 TO PROPER-SPACING
           MOVE ZEROS TO CTL-TOTAL-COST
           MOVE ZEROS TO DF-CANDY-TOTAL
           MOVE CANDY-ID TO DF-CANDY-HOLD
		   
           .

       800-EOF-ROUTINE.

           PERFORM 600-WAREHOUSE-BREAK
 
           .
		   
	   900-PRINT-FINAL-TOTAL.
	   
	       MOVE DF-GRAND-TOTAL TO GTL-TOTAL-COST
		   MOVE GRAND-TOTAL-LINE TO REPORT-RECORD
		   MOVE 3 TO PROPER-SPACING
		   PERFORM 150-WRITE-LINE
		   
		   .

       1000-FINAL-ROUTINE.
    
          CLOSE INVENTORY-FILE
                REPORT-FILE
          STOP RUN
		  
          .
