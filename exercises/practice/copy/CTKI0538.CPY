      ******************************************************************
      * DCLGEN TABLE(T13KIS.G66_PRIVATKREDIT)                          *
      *        LIBRARY(T130005.OZPGEN.SRCDCL(CTKI0538))                *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(G66_PRIVATKREDIT-GESAMT)                      *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE G66_PRIVATKREDIT TABLE
CCC004*    ( G66PVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G66PMANDANT                    INTEGER NOT NULL,
CCC004*      G66PZUSTAND                    CHAR(1) NOT NULL,
CCC004*      G66PBEGINNDATUM                CHAR(8) NOT NULL,
CCC004*      G66PENDEDATUM                  CHAR(8) NOT NULL,
CCC004*      G66PABSCHLVBNR                 INTEGER NOT NULL,
CCC004*      G66PDARLBETR                   DECIMAL(14, 2) NOT NULL,
CCC004*      G66PKREDITZUSDAT               CHAR(8) NOT NULL,
CCC004*      G66PKREDITAUSDAT               CHAR(8) NOT NULL,
CCC004*      G66PKREDITART                  CHAR(6) NOT NULL,
CCC004*      G66PSALDOBETR                  DECIMAL(14, 2) NOT NULL,
CCC004*      G66PZINSSATZ                   DECIMAL(6, 4) NOT NULL,
CCC004*      G66PRATEBETR                   DECIMAL(14, 2) NOT NULL,
CCC004*      G66PFAELLIGDAT                 CHAR(8) NOT NULL,
CCC004*      G66PSICHERHEIT                 CHAR(1) NOT NULL,
CCC004*      G66PRESTLAUFZEIT               CHAR(3) NOT NULL,
CCC004*      G66PIBANBELAST                 CHAR(34) NOT NULL,
CCC004*      G66PBICBELAST                  CHAR(11) NOT NULL,
CCC004*      G66PMANDATSREF                 CHAR(35) NOT NULL,
CCC004*      G66PSEPANACHNAME               CHAR(30) NOT NULL,
CCC004*      G66PSEPAVORNAME                CHAR(30) NOT NULL,
CCC004*      G66PIBANVERTRAG                CHAR(34) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G66_PRIVATKREDIT            *
      ******************************************************************
       01  G66-PRIVATKREDIT-GESAMT.
        05 NI-G66-PRIVATKREDIT-TAB.
           10 NI-G66-PRIVATKREDIT-ELE PIC S9(04) COMP OCCURS 22 TIMES.
        05 NI-G66-PRIVATKREDIT REDEFINES NI-G66-PRIVATKREDIT-TAB.
           10 NI-G66PVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G66PMANDANT        PIC S9(4) COMP.
           10 NI-G66PZUSTAND        PIC S9(4) COMP.
           10 NI-G66PBEGINNDATUM    PIC S9(4) COMP.
           10 NI-G66PENDEDATUM      PIC S9(4) COMP.
           10 NI-G66PABSCHLVBNR     PIC S9(4) COMP.
           10 NI-G66PDARLBETR       PIC S9(4) COMP.
           10 NI-G66PKREDITZUSDAT   PIC S9(4) COMP.
           10 NI-G66PKREDITAUSDAT   PIC S9(4) COMP.
           10 NI-G66PKREDITART      PIC S9(4) COMP.
           10 NI-G66PSALDOBETR      PIC S9(4) COMP.
           10 NI-G66PZINSSATZ       PIC S9(4) COMP.
           10 NI-G66PRATEBETR       PIC S9(4) COMP.
           10 NI-G66PFAELLIGDAT     PIC S9(4) COMP.
           10 NI-G66PSICHERHEIT     PIC S9(4) COMP.
           10 NI-G66PRESTLAUFZEIT   PIC S9(4) COMP.
           10 NI-G66PIBANBELAST     PIC S9(4) COMP.
           10 NI-G66PBICBELAST      PIC S9(4) COMP.
           10 NI-G66PMANDATSREF     PIC S9(4) COMP.
           10 NI-G66PSEPANACHNAME   PIC S9(4) COMP.
           10 NI-G66PSEPAVORNAME    PIC S9(4) COMP.
           10 NI-G66PIBANVERTRAG    PIC S9(4) COMP.
        05 DCL-G66-PRIVATKREDIT.
           10 G66PVERTRAGSNR       PIC X(15).
           10 G66PMANDANT          PIC S9(9) USAGE COMP.
           10 G66PZUSTAND          PIC X(1).
           10 G66PBEGINNDATUM      PIC X(8).
           10 G66PENDEDATUM        PIC X(8).
           10 G66PABSCHLVBNR       PIC S9(9) USAGE COMP.
           10 G66PDARLBETR         PIC S9(12)V9(2) USAGE COMP-3.
           10 G66PKREDITZUSDAT     PIC X(8).
           10 G66PKREDITAUSDAT     PIC X(8).
           10 G66PKREDITART        PIC X(6).
           10 G66PSALDOBETR        PIC S9(12)V9(2) USAGE COMP-3.
           10 G66PZINSSATZ         PIC S9(2)V9(4) USAGE COMP-3.
           10 G66PRATEBETR         PIC S9(12)V9(2) USAGE COMP-3.
           10 G66PFAELLIGDAT       PIC X(8).
           10 G66PSICHERHEIT       PIC X(1).
           10 G66PRESTLAUFZEIT     PIC X(3).
           10 G66PIBANBELAST       PIC X(34).
           10 G66PBICBELAST        PIC X(11).
           10 G66PMANDATSREF       PIC X(35).
           10 G66PSEPANACHNAME     PIC X(30).
           10 G66PSEPAVORNAME      PIC X(30).
           10 G66PIBANVERTRAG      PIC X(34).
      ************************** ENDE-DCLGEN *************************

