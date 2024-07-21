      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.02.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0116.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WVVB0116.CPv  $
      *
      *   Rev 2.2   Jun 30 2016 11:54:36   t13ex71
      *AVB01400: Generierung Lese-OZP, Schreibmodul, etc. für Tabel
      *le TBVB0116 (BPR)
      *
      *   Rev 2.1   Feb 20 2009 08:29:58   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Feb 19 2009 09:34:36   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0116                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0116 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    07.06.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!07.06.16!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0116                           *
      *****************************************************************
 
        05 WB-NI-VIVB0116-TAB.
           10 WB-NI-VIVB0116-ELE   PIC S9(04)  BINARY OCCURS   8.
        05 WB-NI-VIVB0116          REDEFINES   WB-NI-VIVB0116-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-RSTAT-DAT-VON         PIC S9(04)       BINARY.
           10 WB-NI-KNR-RSTAT-FIBU        PIC S9(04)       BINARY.
           10 WB-NI-KTY-RSTAT-FIBU        PIC S9(04)       BINARY.
           10 WB-NI-KNR-RSTAT-DAISY       PIC S9(04)       BINARY.
           10 WB-NI-KTY-RSTAT-DAISY       PIC S9(04)       BINARY.
           10 WB-NI-RSTAT-DAT-BIS         PIC S9(04)       BINARY.
           10 WB-NI-RSTAT-LAST-UPDATE     PIC S9(04)       BINARY.
 
        05 WB-VIVB0116.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-RSTAT-DAT-VON            PIC  X(26).
           10 WB-KNR-RSTAT-FIBU           PIC S9(04)       BINARY.
           10 WB-KTY-RSTAT-FIBU           PIC S9(04)       BINARY.
           10 WB-KNR-RSTAT-DAISY          PIC S9(04)       BINARY.
           10 WB-KTY-RSTAT-DAISY          PIC S9(04)       BINARY.
           10 WB-RSTAT-DAT-BIS            PIC  X(10).
           10 WB-RSTAT-LAST-UPDATE        PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************

