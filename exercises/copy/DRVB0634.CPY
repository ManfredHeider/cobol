      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0634.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0634.CPv  $
      *
      *   Rev 2.2   Oct 29 2009 13:19:18   t13ae13
      *AVB01014: Änderung ED-Regelung ab Alter 60
      *
      *   Rev 2.1   Feb 20 2003 14:43:42   t13ex22
      *AAN00660: Erfassen eMail im VB43
      *
      *   Rev 2.0   Dec 04 2002 12:21:44   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0634                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0081 (veraltet)             *
060709*    SCHNITTSTELLE FÜR DAS UPRO ZXVB0083                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    27.01.2003                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00660!27.01.03! 01 ! NEU                       !MUND      *
      * AVB01014!06.07.09!2.2 ! umbenannt in ZXVB0083     !Walb      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0634                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0634.
          05 PI-DRVB0634-UFKT-CODE             PIC  9(002).
          05 PI-DRVB0634-ST-NR                 PIC S9(009)  COMP.

      *
       01 PO-DRVB0634.
          05 PO-DRVB0634-RC                    PIC  X(003).
             88 PO-DRVB0634-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0634-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0634-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0634-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0634-DATEN.
             10 PO-DRVB0634-MA-ST-NR           PIC S9(9)   COMP.
             10 PO-DRVB0634-MEMZ-DAT-VON       PIC X(26).
             10 PO-DRVB0634-MEMZ-SST-KZ        PIC X(01).
             10 PO-DRVB0634-MEMZ-LAST-UPDATE   PIC X(26).
             10 PO-DRVB0634-MEMZ-SST-DAT       PIC X(10).
             10 PO-DRVB0634-MEMZ-DAT-BIS       PIC X(10).
             10 PO-DRVB0634-MEMZ-EMAIL         PIC X(70).
      *
      ***** ENDE COPY-MEMBER DRVB0634 *****************************
