      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0951.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0951.CPv  $
      *
      *   Rev 2.1   Oct 29 2009 13:19:40   t13ae13
      *AVB01014: Änderung ED-Regelung ab Alter 60
      *
      *   Rev 2.0   Aug 18 2009 10:03:28   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0951                                      *
      *    --------------------------------------------------------   *
      *    ÜBERGABEBEREICH VOM VB29 an Unterdialog XA20               *
      *    (ACHTUNG: BEGINNT AUF STUFE 08)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    18.08.2009                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01014!18.08.09!2.1 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      ****************************************************************
      * 01 ---> KEINE 01-ER-STUFE, DAMIT DIESE STRUKTUR
      *    --->  AUCH IN CA-SICHERUNG ÜBERNOMMEN WERDEN KANN

           08  DRVB0951.
               10  DRVB0951-ED-ZWEIG               PIC  9(007).
               10  DRVB0951-ED-ZWEIG-VOM           PIC  X(010).


      * (ACHTUNG: MAX 128 BYTES, DA ÜBER CADD-I-KOMM-VAR ÜBERTRAGEN !!)
      *
      ******** ENDE COPY-MEMBER (DRVB0951) *******************

