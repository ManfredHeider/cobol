      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0987                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0096                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    11.08.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00352!11.08.10!2.0 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *                                                               *
      * VLA-1995!12.03.21! NEUE VERSION EBB WG. KS2021    ! T13AE30   *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0987                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0987.
          05 PI-DRVB0987-MA-ST-NR            PIC S9(009) BINARY.
          05 PI-DRVB0987-KNR-TYP-BEFBED      PIC S9(004) BINARY.
      *
       01 PO-DRVB0987.
          05 PO-DRVB0987-RC                  PIC  X(003).
             88 PO-DRVB0987-RC-OK                VALUE 'OK '.
             88 PO-DRVB0987-RC-NOT-FOUND         VALUE 'NFD'.
         03 PO-DRVB0987-DATEN.                                                  
          05 PO-DRVB0987-NI-VIVB0096.                                           
             10 NI-DRVB0987-MA-ST-NR           PIC S9(04)       BINARY.         
             10 NI-DRVB0987-KTY-TYP-BEFBED     PIC S9(04)       BINARY.         
             10 NI-DRVB0987-KNR-TYP-BEFBED     PIC S9(04)       BINARY.         
             10 NI-DRVB0987-LAUF-DAT           PIC S9(04)       BINARY.         
             10 NI-DRVB0987-STF-AKT            PIC S9(04)       BINARY.         
             10 NI-DRVB0987-STF-SEIT           PIC S9(04)       BINARY.         
             10 NI-DRVB0987-ZPKT-VON           PIC S9(04)       BINARY.         
             10 NI-DRVB0987-ZPKT-BIS           PIC S9(04)       BINARY.         
             10 NI-DRVB0987-EH-EIG             PIC S9(04)       BINARY.         
             10 NI-DRVB0987-EH-EIG-KZ          PIC S9(04)       BINARY.         
             10 NI-DRVB0987-EQF-EIG            PIC S9(04)       BINARY.         
             10 NI-DRVB0987-EQF-EIG-KZ         PIC S9(04)       BINARY.         
             10 NI-DRVB0987-EH-GES             PIC S9(04)       BINARY.         
             10 NI-DRVB0987-KORR-EH-GES        PIC S9(04)       BINARY.         
             10 NI-DRVB0987-KORR-EH-GES-KZ     PIC S9(04)       BINARY.         
             10 NI-DRVB0987-EQF-GES            PIC S9(04)       BINARY.         
             10 NI-DRVB0987-EQF-GES-KZ         PIC S9(04)       BINARY.         
             10 NI-DRVB0987-EH-NEU-PART        PIC S9(04)       BINARY.         
             10 NI-DRVB0987-EH-PART-KZ         PIC S9(04)       BINARY.         
             10 NI-DRVB0987-PROZ-KLAUS         PIC S9(04)       BINARY.         
             10 NI-DRVB0987-EIGEN-MAX          PIC S9(04)       BINARY.         
             10 NI-DRVB0987-GESAMT-KZ          PIC S9(04)       BINARY.         
             10 NI-DRVB0987-LAST-UPDATE        PIC S9(04)       BINARY.         
          05 PO-DRVB0987-DCL-VIVB0096.                                          
             10 PO-DRVB0987-MA-ST-NR           PIC S9(09)       BINARY.         
             10 PO-DRVB0987-KTY-TYP-BEFBED     PIC S9(04)       BINARY.         
             10 PO-DRVB0987-KNR-TYP-BEFBED     PIC S9(04)       BINARY.         
             10 PO-DRVB0987-LAUF-DAT           PIC  X(10).                      
             10 PO-DRVB0987-STF-AKT            PIC S9(04)       BINARY.         
             10 PO-DRVB0987-STF-SEIT           PIC  X(10).                      
             10 PO-DRVB0987-ZPKT-VON           PIC  X(10).                      
             10 PO-DRVB0987-ZPKT-BIS           PIC  X(10).                      
             10 PO-DRVB0987-EH-EIG             PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0987-EH-EIG-KZ          PIC  X(01).                      
             10 PO-DRVB0987-EQF-EIG            PIC S9(03)V9(02) COMP-3.         
             10 PO-DRVB0987-EQF-EIG-KZ         PIC  X(01).                      
             10 PO-DRVB0987-EH-GES             PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0987-KORR-EH-GES        PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0987-KORR-EH-GES-KZ     PIC  X(01).                      
             10 PO-DRVB0987-EQF-GES            PIC S9(03)V9(02) COMP-3.         
             10 PO-DRVB0987-EQF-GES-KZ         PIC  X(01).                      
             10 PO-DRVB0987-EH-NEU-PART        PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0987-EH-PART-KZ         PIC  X(01).                      
             10 PO-DRVB0987-PROZ-KLAUS         PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0987-EIGEN-MAX          PIC S9(09)V9(01) COMP-3.         
             10 PO-DRVB0987-GESAMT-KZ          PIC  X(01).                      
             10 PO-DRVB0987-LAST-UPDATE        PIC  X(26).                      
      *
      ***** ENDE COPY-MEMBER DRVB0987 *****************************
