      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0653.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0653.CPv  $
      *
      *   Rev 2.2   Sep 15 2016 12:29:02   t13ae13
      *AVB01400: Projekt Lucy2 - Anpassung SL02 (SWL)
      *
      *   Rev 2.1   Apr 23 2003 16:13:36   t13ex22
      *ASL00156: Neuer Dialog SL01
      *
      *   Rev 2.0   Feb 17 2003 10:57:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0653                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0088                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    11.12.2002                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00153!11.12.02! 2.1! NEU, BOZ-ANSPRÜCHE        ! J.HERING *
      *         !        !    ! JE MONAT                  !          *
      * --------+--------+----+---------------------------+----------*
      * AVB01400 13.06.16!2.2 ! neues Feld                ! S. Walb  *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0653                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0653.
          05 PI-DRVB0653-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0653-ANSPR-VOM          PIC  X(010).
          05 PI-DRVB0653-ANSPR-STATUS       PIC  S9(004) BINARY.
          05 PI-DRVB0653-BOZA-ST-NR         PIC  S9(009) BINARY.
      *
       01 PO-DRVB0653.

          05 PO-DRVB0653-RC                 PIC  X(003).
             88 PO-DRVB0653-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0653-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0653-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0653-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0653-DATEN.
             10 PO-DRVB0653-BOZA-ANSPR-VOM     PIC  X(010).
             10 PO-DRVB0653-BOZA-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0653-KNR-WAEHRUNG       PIC  X(003).
130616       10 PO-DRVB0653-BOZA-ANSP-BET-UPFL PIC S9(09)V9(02) COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0653 *****************************


