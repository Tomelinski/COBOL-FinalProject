       identification division.
       program-id. Final-DataSplitAndCount.
       author. name. Tom Zielinski.
       date-written. date. 03/30/2021
      *Last-Edited:        04/15/2021
      *Description: This program takes validated records and sorts and
      *            splits the data into (S and L).dat and R.dat files.
      *            it also produces a seperate summary based on the 
      *            processed records.


       environment division.

       input-output section.
       file-control.

           select data-file
               assign to "../../../../data/Final-Valid.dat"
                  organization is line sequential.

           select return-file
               assign to "../../../../data/Final-return-records.dat"
               organization is line sequential.

           select sl-file
               assign to "../../../../data/Final-sl-records.dat"
               organization is line sequential.

           select summary-file
               assign to "../../../../data/Final-summary.out"
               organization is line sequential.     

       configuration section.

       data division.
       file section.
       fd data-file
           data record is item-rec
           record contains 36 characters.
      *
       
       01 item-rec.
         05 it-code                 pic x.
           88 it-code-l                 value 'L'.
           88 it-code-s                 value 'S'.
           88 it-code-r                 value 'R'.
         05 it-transaction          pic 9(5)v99.
         05 it-payment-type         pic xx.
           88 it-ca                     value 'CA'.
           88 it-cr                     value 'CR'.
           88 it-db                     value 'DB'.
         05 it-store-num            pic 99.
           88 it-store-1                value 1.
           88 it-store-2                value 2.
           88 it-store-3                value 3.
           88 it-store-4                value 4.
           88 it-store-5                value 5.
           88 it-store-12               value 12.
         05 it-invoice              pic x(9).
         05 it-sku-code             pic x(15).         

      *
       fd return-file
           data record is return-line
           record contains 36 characters.

       01 return-line               pic x(36).

       fd sl-file
           data record is sl-line
           record contains 36 characters.

       01 sl-line                   pic x(36).

       fd summary-file
           data record is summary-line
           record contains 81 characters.

       01 summary-line              pic x(81).

       working-storage section.

       01 ws-heading1-name-line.
         05 filler                  pic x(25)
               value "Tom Zielinski, Calvin May".
         05 filler                  pic x(11)   value space.
         05 filler                  pic x(33)
               value "Final: Program 2 - Data Splitting".

       01 ws-heading2-title.
         05 filler                  pic x(31)   value spaces.
         05 filler                  pic x(14)   value "SUMMARY REPORT".

       01 ws-heading3-headings.
         05 filler                  pic x(5)    value spaces.
         05 filler                  pic x(22)
               value "TRANSACTIONS PER STORE".
         

       01 ws-heading4-headings.
         05 filler                  pic x(5)    value spaces.
         05 filler                  pic x(17)
               value "RETURNS PER STORE".


       01 ws-summary-line1.
         05 filler                  pic x(17)
               value "TRANSACTION CODE:"                                .
         05 filler                  pic x(3)    value spaces.
         05 filler                  pic x(9)    value "S Records".
         05 filler                  pic x(3)    value spaces.
         05 filler                  pic x(9)    value "L Records".
         05 filler                  pic x(3)    value spaces.
         05 filler                  pic x(13)   value "S & L Records".
         05 filler                  pic x(3)    value spaces.
         05 filler                  pic x(9)    value "R Records".
                                    
       01 ws-summary-line2.         
         05 filler                  pic x(23)   value spaces.
         05 ws-total-s-edit         pic zz9.
         05 filler                  pic x(9)    value spaces.
         05 ws-total-l-edit         pic zz9.
         05 filler                  pic x(11)   value spaces.
         05 ws-total-sl-edit        pic zz9.
         05 filler                  pic x(11)   value spaces.
         05 ws-total-return-edit    pic zz9.
                                    
       01 ws-summary-line3.         
         05 filler                  pic x(43)    value spaces.
         05 filler                  pic x(23)
               value "TOTAL RECORDS =".
         05 ws-total-records-edit   pic zz9.
                                    
       01 ws-summary-line4.         
         05 filler                  pic x(13)   value "PAYMENT TYPE:".
         05 filler                  pic x(6)    value spaces.
         05 filler                  pic x(4)    value "Cash".
         05 filler                  pic x(6)    value spaces.
         05 filler                  pic x(6)    value "Credit".
         05 filler                  pic x(6)    value spaces.
         05 filler                  pic x(5)    value "Debit".
                                    
       01 ws-summary-line5.         
         05 filler                  pic x(18)   value "S RECORDS:".
         05 ws-percent-s-ca-edit    pic z9.99.
         05 filler                  pic x(6)    value "%".
         05 ws-percent-s-cr-edit    pic z9.99.
         05 filler                  pic x(7)    value "%".
         05 ws-percent-s-db-edit    pic z9.99.
         05 filler                  pic x(1)    value "%".
                                    
       01 ws-summary-line6.         
         05 filler                  pic x(18)   value "L RECORDS:".
         05 ws-percent-l-ca-edit    pic z9.99.
         05 filler                  pic x(6)    value "%".
         05 ws-percent-l-cr-edit    pic z9.99.
         05 filler                  pic x(7)    value "%".
         05 ws-percent-l-db-edit    pic z9.99.
         05 filler                  pic x(1)    value "%".
                                    
       01 ws-s-payment.             
         05 ws-s-ca                 pic 99.
         05 ws-s-cr                 pic 99.
         05 ws-s-db                 pic 99.
         05 ws-percent-s-ca         pic 99v99.
         05 ws-percent-s-cr         pic 99v99.
         05 ws-percent-s-db         pic 99v99.
                                    
       01 ws-l-payment.             
         05 ws-l-ca                 pic 99.
         05 ws-l-cr                 pic 99.
         05 ws-l-db                 pic 99.
         05 ws-percent-l-ca         pic 99v99.
         05 ws-percent-l-cr         pic 99v99.
         05 ws-percent-l-db         pic 99v99.
                                    
       01 ws-totals.                
         05 ws-total-records        pic 999     value 0.
         05 ws-total-s              pic 999     value 0.
         05 ws-total-l              pic 999     value 0.
         05 ws-total-sl             pic 999     value 0.
         05 ws-total-return         pic 999     value 0.
                                    
       01 ws-transaction            pic 9(6)v99.

       01 ws-display-total-trans.
         05 filler                  pic x(6)    value "STORE ".
         05 ws-store-num-trans-edit pic z9.
         05 filler                  pic x(2)    value ": ".
         05 ws-store-trans-edit     pic zzz,zz9.99.

       01 ws-display-total-r.
         05 filler                  pic x(6)    value "STORE ".
         05 ws-store-num-r-edit     pic z9.
         05 filler                  pic x(2)     value ": ".
         05 ws-r-count-edit         pic z9.
         
       01 ws-stores.
         05 ws-store occurs 6 times.
           10 ws-store-num          pic 99
                   value 01, 02, 03, 04, 05, 12.
           10 ws-store-trans        pic 9(6)v99.
           10 ws-r-count            pic 99      value 0.
                                    
       01 ws-flags.                 
         05 ws-eof-flag             pic x       value 'n'.
         05 ws-new-page             pic x       value 'y'.
                                    
       01 ws-counters.              
         05 ws-page-count           pic 9       value 1.
         05 ws-line-count           pic S99     value 0.
         05 ws-index                pic 99      value 1.
                                    
       77 ws-lines-per-page         pic 99      value 15.
       77 ws-zero                   pic 9       value 0.
       77 ws-one                    pic 9       value 1.
       77 ws-two                    pic 9       value 2.
       77 ws-three                  pic 9       value 3.
       77 ws-four                   pic 9       value 4.
       77 ws-five                   pic 9       value 5.
       77 ws-six                    pic 9       value 6.
       77 ws-one-hundred            pic 999     value 100.
       77 ws-y                      pic x       value "y".

       procedure division.
       000-main.
      *Open read/write files
           open input  data-file.
           open output return-file,
                       sl-file,
                       summary-file.

      *set end of file variable
           perform 010-read-file.


           perform 200-print-headings.

      *read file until end of file is reached
      *also, call calculate and display functions
           perform 100-process-pages
               until ws-eof-flag equals ws-y.

           perform 330-calculate-percents.

           perform 400-report-footer.

           close data-file,
                 return-file,
                 sl-file,
                 summary-file.
           goback.

       010-read-file.
           read data-file
               at end
                   move ws-y to ws-eof-flag.

       100-process-pages.
      *print lines per page 
           perform 300-process-lines
             varying ws-line-count from ws-zero by ws-one
               until ws-line-count equals ws-lines-per-page
                   or ws-eof-flag  equals ws-y.

      *increase page number by 1 after page has been read
           add ws-one          to ws-page-count.
           move ws-y           to ws-new-page.



       200-print-headings.
      *display name and header
           write summary-line  from ws-heading1-name-line
             after advancing ws-one line.
           write summary-line  from ws-heading2-title
             after advancing ws-two line.



       300-process-lines.
      *reset variables

           add ws-one  to ws-total-records.

           if (it-code-r) then
               perform 310-return-record
           else
               perform 320-sl-record
           end-if.

      *

      *check if end of file 
           perform 010-read-file.


       310-return-record.
       
      *add to summary variables and totals
           add ws-one              to ws-total-return.
                             
           if it-store-1  then
               add ws-one          to ws-r-count(ws-one)
           else              
           if it-store-2  then
               add ws-one          to ws-r-count(ws-two)
           else              
           if it-store-3  then
               add ws-one          to ws-r-count(ws-three)
           else              
           if it-store-4  then
               add ws-one          to ws-r-count(ws-four)
           else              
           if it-store-5  then
               add ws-one          to ws-r-count(ws-five)
           else              
           if it-store-12 then
               add ws-one          to ws-r-count(ws-six)
           end-if
           end-if
           end-if
           end-if
           end-if
           end-if.

           write return-line from item-rec.



       320-sl-record.
           move it-transaction     to ws-transaction.
       
      * Count which type of record this is 
           if (it-code-s) then
               add ws-one          to ws-total-s   

               if it-ca   then
                   add ws-one      to ws-s-ca
               else
               if it-cr   then
                   add ws-one      to ws-s-cr
               else
               if it-db   then
                   add ws-one      to ws-s-db
               end-if
               end-if
               end-if
           else
               add ws-one          to ws-total-l   

               if it-ca   then
                   add ws-one      to ws-l-ca
               else
               if it-cr   then
                   add ws-one      to ws-l-cr
               else
               if it-db   then
                   add ws-one      to ws-l-db
               end-if
               end-if
               end-if
           end-if.

           if it-store-1  then
               add ws-transaction  to ws-store-trans(ws-one)
           else
           if it-store-2  then
               add ws-transaction  to ws-store-trans(ws-two)
           else
           if it-store-3  then
               add ws-transaction  to ws-store-trans(ws-three)
           else
           if it-store-4  then
               add ws-transaction  to ws-store-trans(ws-four)
           else
           if it-store-5  then
               add ws-transaction  to ws-store-trans(ws-five)
           else
           if it-store-12  then
               add ws-transaction  to ws-store-trans(ws-six)
           end-if
           end-if
           end-if
           end-if
           end-if
           end-if.
       
           write sl-line from item-rec.

       330-calculate-percents.
           compute ws-percent-s-ca rounded =
             (ws-s-ca / ws-total-s) * ws-one-hundred.
           compute ws-percent-s-cr rounded =
             (ws-s-cr / ws-total-s) * ws-one-hundred.
           compute ws-percent-s-db rounded =
             (ws-s-db / ws-total-s) * ws-one-hundred.

           compute ws-percent-l-ca rounded =
             (ws-l-ca / ws-total-l) * ws-one-hundred.
           compute ws-percent-l-cr rounded =
             (ws-l-cr / ws-total-l) * ws-one-hundred.
           compute ws-percent-l-db rounded =
             (ws-l-db / ws-total-l) * ws-one-hundred.

           move ws-percent-s-ca    to ws-percent-s-ca-edit.
           move ws-percent-s-cr    to ws-percent-s-cr-edit.
           move ws-percent-s-db    to ws-percent-s-db-edit.

           move ws-percent-l-ca    to ws-percent-l-ca-edit.
           move ws-percent-l-cr    to ws-percent-l-cr-edit.
           move ws-percent-l-db    to ws-percent-l-db-edit.

       400-report-footer.
           move ws-total-records   to ws-total-records-edit.
      *    Calculate total sl records
           compute ws-total-sl = (ws-total-s + ws-total-l).

      *Move summary vaiables into display variables
           move ws-total-s         to ws-total-s-edit.
           move ws-total-l         to ws-total-l-edit.
           move ws-total-sl        to ws-total-sl-edit.

           move ws-total-sl        to ws-total-sl-edit.
           move ws-total-return    to ws-total-return-edit.


      *Display data for S&L records
           write summary-line from ws-summary-line1
             after advancing ws-two line.
           write summary-line from ws-summary-line2
             after advancing ws-one line.
           write summary-line from ws-summary-line3
             after advancing ws-one line.


           write summary-line from ws-heading3-headings
             after advancing ws-two line.
           write summary-line from spaces.
           
           perform 400-display-store-trans
             varying ws-index from ws-one by ws-one
             until ws-index > ws-six.

           write summary-line from ws-summary-line4
             after advancing ws-two lines.
           write summary-line from ws-summary-line5
             after advancing ws-one lines.
           write summary-line from ws-summary-line6
             after advancing ws-one lines.

      *Display data for R records

           write summary-line from ws-heading4-headings
             after advancing ws-two line.
           write summary-line from spaces.

           perform 400-display-store-r
             varying ws-index from ws-one by ws-one
             until ws-index > ws-six.

      *    write summary-line from ws-line5-total
      *      after advancing ws-one line.

       400-display-store-trans.
           move ws-store-num(ws-index)
             to ws-store-num-trans-edit.
           move ws-store-trans(ws-index)
             to ws-store-trans-edit.
           write summary-line from ws-display-total-trans.

       400-display-store-r.
           move ws-store-num(ws-index)
             to ws-store-num-r-edit.
           move ws-r-count(ws-index)
             to ws-r-count-edit.
           write summary-line from ws-display-total-r.

       end program Final-DataSplitAndCount.