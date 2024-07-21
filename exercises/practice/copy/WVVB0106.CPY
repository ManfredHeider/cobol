      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.03.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0106.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0106.CPv  $
      *
      *   Rev 2.1   05 Jul 2007 16:02:06   t13ex13
      *AVB00906: Einführung EU-Verm.RL im VB-System: Datenbasis
      *
      *   Rev 2.0   21 Mar 2007 15:11:24   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: WVVB0106                                     *
      *    --------------------------------------------------------  *
      *    WORKBEREICH FÜR VIVB0106 (REG)                            *
      *    --------------------------------------------------------  *
      *    ERSTELLT: RALF GUTFLEISCH                                 *
      *    DATUM:    21.03.2007                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00906!21.03.07! 2.0! NEU                       !GUTFLEISCH*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0106                          *
      *****************************************************************
        05  WB-NI-VIVB0106-TAB.
           10  WB-NI-VIVB0106-ELE  PIC S9(04)  COMP   OCCURS 13 TIMES.
        05  WB-NI-VIVB0106           REDEFINES   WB-NI-VIVB0106-TAB.
           10 WB-NI-REG-HAFT-AB        PIC S9(4) USAGE COMP.
           10 WB-NI-REG-NR             PIC S9(4) USAGE COMP.
           10 WB-NI-REG-NAME           PIC S9(4) USAGE COMP.
           10 WB-NI-REG-VORNAME        PIC S9(4) USAGE COMP.
           10 WB-NI-REG-PLZ            PIC S9(4) USAGE COMP.
           10 WB-NI-REG-ORT            PIC S9(4) USAGE COMP.
           10 WB-NI-REG-STR            PIC S9(4) USAGE COMP.
           10 WB-NI-REG-GEBDAT         PIC S9(4) USAGE COMP.
           10 WB-NI-REG-REGISTER-TS    PIC S9(4) USAGE COMP.
           10 WB-NI-REG-TAET-LAND      PIC S9(4) USAGE COMP.
           10 WB-NI-REG-CODE           PIC S9(4) USAGE COMP.
           10 WB-NI-REG-IHK-NR         PIC S9(4) USAGE COMP.
           10 WB-NI-REG-CODE-TEXT      PIC S9(4) USAGE COMP.
        05  WB-VIVB0106.
           10 WB-MA-ST-NR                  PIC S9(9) COMP   VALUE ZERO.
           10 WB-REG-LAST-UPDATE           PIC X(26)        VALUE SPACE.
           10 WB-REG-DAT-VON               PIC X(26)        VALUE SPACE.
           10 WB-REG-HAFT-AB               PIC X(10)        VALUE SPACE.
           10 WB-REG-NR                    PIC X(30)        VALUE SPACE.
           10 WB-REG-NAME                  PIC X(50)        VALUE SPACE.
           10 WB-REG-VORNAME               PIC X(50)        VALUE SPACE.
           10 WB-REG-PLZ                   PIC X(10)        VALUE SPACE.
           10 WB-REG-ORT                   PIC X(50)        VALUE SPACE.
           10 WB-REG-STR                   PIC X(50)        VALUE SPACE.
           10 WB-REG-GEBDAT                PIC X(10)        VALUE SPACE.
           10 WB-REG-AKTION                PIC X(03)        VALUE SPACE.
           10 WB-REG-STATUS                PIC X(06)        VALUE SPACE.
           10 WB-REG-REGISTER-TS           PIC X(35)        VALUE SPACE.
           10 WB-REG-TAET-LAND             PIC X(60)        VALUE SPACE.
           10 WB-REG-CODE                  PIC S9(08) COMP  VALUE ZERO.
           10 WB-REG-ART                   PIC X(001)       VALUE SPACE.
           10 WB-REG-IHK-NR                PIC S9(04) COMP  VALUE ZERO.
           10 WB-REG-BUCH-KZ               PIC X(001)       VALUE SPACE.
           10 WB-REG-CODE-TEXT             PIC X(255)       VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
