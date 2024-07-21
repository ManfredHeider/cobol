      ******************************************************************
      * DCLGEN TABLE(T13KIS.G66_KUNDE)                                 *
      *        LIBRARY(T130005.OZPGEN.SRCDCL(CTKI0537))                *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(G66_KUNDE-GESAMT)                             *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE G66_KUNDE TABLE
CCC004*    ( G66KVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G66KLFDNR                      SMALLINT NOT NULL,
CCC004*      G66KNAME                       CHAR(35) NOT NULL,
CCC004*      G66KVORNAME                    CHAR(35) NOT NULL,
CCC004*      G66KGEBURTSDATUM               CHAR(8) NOT NULL,
CCC004*      G66KANREDE                     CHAR(1) NOT NULL,
CCC004*      G66KSTRHNR                     CHAR(40) NOT NULL,
CCC004*      G66KPLZ                        CHAR(9) NOT NULL,
CCC004*      G66KORT                        CHAR(30) NOT NULL,
CCC004*      G66KISOCODE                    CHAR(3) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G66_KUNDE                   *
      ******************************************************************
       01  G66-KUNDE-GESAMT.
        05 NI-G66-KUNDE-TAB.
           10 NI-G66-KUNDE-ELE PIC S9(04) COMP OCCURS 10 TIMES.
        05 NI-G66-KUNDE REDEFINES NI-G66-KUNDE-TAB.
           10 NI-G66KVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G66KLFDNR          PIC S9(4) COMP.
           10 NI-G66KNAME           PIC S9(4) COMP.
           10 NI-G66KVORNAME        PIC S9(4) COMP.
           10 NI-G66KGEBURTSDATUM   PIC S9(4) COMP.
           10 NI-G66KANREDE         PIC S9(4) COMP.
           10 NI-G66KSTRHNR         PIC S9(4) COMP.
           10 NI-G66KPLZ            PIC S9(4) COMP.
           10 NI-G66KORT            PIC S9(4) COMP.
           10 NI-G66KISOCODE        PIC S9(4) COMP.
        05 DCL-G66-KUNDE.
           10 G66KVERTRAGSNR       PIC X(15).
           10 G66KLFDNR            PIC S9(4) USAGE COMP.
           10 G66KNAME             PIC X(35).
           10 G66KVORNAME          PIC X(35).
           10 G66KGEBURTSDATUM     PIC X(8).
           10 G66KANREDE           PIC X(1).
           10 G66KSTRHNR           PIC X(40).
           10 G66KPLZ              PIC X(9).
           10 G66KORT              PIC X(30).
           10 G66KISOCODE          PIC X(3).
      ************************** ENDE-DCLGEN *************************

