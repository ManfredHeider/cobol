      *   Mainframe Express 3.1  6.018.0057
      *   Micro Focus BMS Screen Painter (ver IDE_6-0-65)
      *   MapSet Name   BAT0025
      *   Date Created  08/30/2018
      *   Time Created  08:53:16

      *  Input Data For Map BAT0025
         01 BAT0025I.
            03 FILLER                         PIC X(12).
            03 BHEAD1L                        PIC S9(4) COMP.
            03 BHEAD1F                        PIC X.
            03 FILLER                         PIC X(1).
            03 BHEAD1I                        PIC X(35).
            03 DFHG001I                       OCCURS 11 TIMES.
               05 ATRANS1L                       PIC S9(4) COMP.
               05 ATRANS1F                       PIC X.
               05 FILLER                         PIC X(1).
               05 ATRANS1I                       PIC X(1).
               05 ATEXT1L                        PIC S9(4) COMP.
               05 ATEXT1F                        PIC X.
               05 FILLER                         PIC X(1).
               05 ATEXT1I                        PIC X(30).
               05 ASTATU1L                       PIC S9(4) COMP.
               05 ASTATU1F                       PIC X.
               05 FILLER                         PIC X(1).
               05 ASTATU1I                       PIC X(3).
            03 DFHG002I                       OCCURS 11 TIMES.
               05 BTRANS1L                       PIC S9(4) COMP.
               05 BTRANS1F                       PIC X.
               05 FILLER                         PIC X(1).
               05 BTRANS1I                       PIC X(1).
               05 BTEXT1L                        PIC S9(4) COMP.
               05 BTEXT1F                        PIC X.
               05 FILLER                         PIC X(1).
               05 BTEXT1I                        PIC X(30).
               05 BSTATU1L                       PIC S9(4) COMP.
               05 BSTATU1F                       PIC X.
               05 FILLER                         PIC X(1).
               05 BSTATU1I                       PIC X(3).
            03 ATXFCL                         PIC S9(4) COMP.
            03 ATXFCF                         PIC X.
            03 FILLER                         PIC X(1).
            03 ATXFCI                         PIC X(32).

      *  Output Data For Map BAT0025
         01 BAT0025O REDEFINES BAT0025I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(2).
            03 BHEAD1A                        PIC X.
            03 BHEAD1H                        PIC X.
            03 BHEAD1O                        PIC X(35).
            03 DFHG001O OCCURS 11.
               05 FILLER                         PIC X(2).
               05 ATRANS1A                       PIC X.
               05 ATRANS1H                       PIC X.
               05 ATRANS1O                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 ATEXT1A                        PIC X.
               05 ATEXT1H                        PIC X.
               05 ATEXT1O                        PIC X(30).
               05 FILLER                         PIC X(2).
               05 ASTATU1A                       PIC X.
               05 ASTATU1H                       PIC X.
               05 ASTATU1O                       PIC X(3).
            03 DFHG002O OCCURS 11.
               05 FILLER                         PIC X(2).
               05 BTRANS1A                       PIC X.
               05 BTRANS1H                       PIC X.
               05 BTRANS1O                       PIC X(1).
               05 FILLER                         PIC X(2).
               05 BTEXT1A                        PIC X.
               05 BTEXT1H                        PIC X.
               05 BTEXT1O                        PIC X(30).
               05 FILLER                         PIC X(2).
               05 BSTATU1A                       PIC X.
               05 BSTATU1H                       PIC X.
               05 BSTATU1O                       PIC X(3).
            03 FILLER                         PIC X(2).
            03 ATXFCA                         PIC X.
            03 ATXFCH                         PIC X.
            03 ATXFCO                         PIC X(32).


