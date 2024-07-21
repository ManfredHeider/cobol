      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.09.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1172.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1172.CPv  $
      *
      *   Rev 2.2   Nov 27 2017 13:40:32   t13ex71
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - steuerl.
      * Status von Zeitpunkt '2' auf '5' in UPVB0189 und ZWVB0072 (
      *
      *
      *   Rev 2.1   Nov 27 2017 13:22:30   t13ex71
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - steuerl.
      * Status von Zeitpunkt '2' auf '5' in UPVB0189 und ZWVB0072 (
      *
      *
      *   Rev 2.0   Sep 05 2017 15:34:48   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1172                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0072                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.Pretsch                                        *
      *    DATUM:    24.05.20016                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!24.05.16! 01 ! NEU                       !B.PRETSCH *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB1172                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1172.
          05 PI-DRVB1172-KNR-STEUER-INFO    PIC S9(004) COMP.
 
      *
       01 PO-DRVB1172.
 
          05 PO-DRVB1172-RC                 PIC  X(003).
             88 PO-DRVB1172-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1172-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1172-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1172-RC-NOT-FOUND                 VALUE 'NFD'.
 
          05 PO-DRVB1172-DATEN.
             10 PO-DRVB1172-ZEITPUNKT        PIC X(026).
 
      *
      ***** ENDE COPY-MEMBER DRVB1172 *****************************
 
 

