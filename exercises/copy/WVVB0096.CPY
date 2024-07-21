      *****************************************************************         
      *    COPY-MEMBER: WVVB0096                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIVB0096 (EBB     )                       *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: T13AE30                                          *         
      *    DATUM:    05.03.2021                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *         
      * --------+--------+----+---------------------------+-----------*         
      * VLA-1995!05.03.21!2.1 ! NEU                       ! T13AE30   *         
      *         !        !    !                           !           *         
      *         !        !    !                           !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0096                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIVB0096-TAB.                                                  
           10 WB-NI-VIVB0096-ELE   PIC S9(04)  BINARY OCCURS  23.               
        05 WB-NI-VIVB0096          REDEFINES   WB-NI-VIVB0096-TAB.              
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.              
           10 WB-NI-KTY-TYP-BEFBED        PIC S9(04)       BINARY.              
           10 WB-NI-KNR-TYP-BEFBED        PIC S9(04)       BINARY.              
           10 WB-NI-LAUF-DAT              PIC S9(04)       BINARY.              
           10 WB-NI-STF-AKT               PIC S9(04)       BINARY.              
           10 WB-NI-STF-SEIT              PIC S9(04)       BINARY.              
           10 WB-NI-ZPKT-VON              PIC S9(04)       BINARY.              
           10 WB-NI-ZPKT-BIS              PIC S9(04)       BINARY.              
           10 WB-NI-EH-EIG                PIC S9(04)       BINARY.              
           10 WB-NI-EH-EIG-KZ             PIC S9(04)       BINARY.              
           10 WB-NI-EQF-EIG               PIC S9(04)       BINARY.              
           10 WB-NI-EQF-EIG-KZ            PIC S9(04)       BINARY.              
           10 WB-NI-EH-GES                PIC S9(04)       BINARY.              
           10 WB-NI-KORR-EH-GES           PIC S9(04)       BINARY.              
           10 WB-NI-KORR-EH-GES-KZ        PIC S9(04)       BINARY.              
           10 WB-NI-EQF-GES               PIC S9(04)       BINARY.              
           10 WB-NI-EQF-GES-KZ            PIC S9(04)       BINARY.              
           10 WB-NI-EH-NEU-PART           PIC S9(04)       BINARY.              
           10 WB-NI-EH-PART-KZ            PIC S9(04)       BINARY.              
           10 WB-NI-PROZ-KLAUS            PIC S9(04)       BINARY.              
           10 WB-NI-EIGEN-MAX             PIC S9(04)       BINARY.              
           10 WB-NI-GESAMT-KZ             PIC S9(04)       BINARY.              
           10 WB-NI-LAST-UPDATE           PIC S9(04)       BINARY.              
                                                                                
        05 WB-VIVB0096.                                                         
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.              
           10 WB-KTY-TYP-BEFBED           PIC S9(04)       BINARY.              
           10 WB-KNR-TYP-BEFBED           PIC S9(04)       BINARY.              
           10 WB-LAUF-DAT                 PIC  X(10).                           
           10 WB-STF-AKT                  PIC S9(04)       BINARY.              
           10 WB-STF-SEIT                 PIC  X(10).                           
           10 WB-ZPKT-VON                 PIC  X(10).                           
           10 WB-ZPKT-BIS                 PIC  X(10).                           
           10 WB-EH-EIG                   PIC S9(09)V9(01) COMP-3.              
           10 WB-EH-EIG-KZ                PIC  X(01).                           
           10 WB-EQF-EIG                  PIC S9(03)V9(02) COMP-3.              
           10 WB-EQF-EIG-KZ               PIC  X(01).                           
           10 WB-EH-GES                   PIC S9(09)V9(01) COMP-3.              
           10 WB-KORR-EH-GES              PIC S9(09)V9(01) COMP-3.              
           10 WB-KORR-EH-GES-KZ           PIC  X(01).                           
           10 WB-EQF-GES                  PIC S9(03)V9(02) COMP-3.              
           10 WB-EQF-GES-KZ               PIC  X(01).                           
           10 WB-EH-NEU-PART              PIC S9(09)V9(01) COMP-3.              
           10 WB-EH-PART-KZ               PIC  X(01).                           
           10 WB-PROZ-KLAUS               PIC S9(09)V9(01) COMP-3.              
           10 WB-EIGEN-MAX                PIC S9(09)V9(01) COMP-3.              
           10 WB-GESAMT-KZ                PIC  X(01).                           
           10 WB-LAST-UPDATE              PIC  X(26).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  23                    *         
      *****************************************************************         
