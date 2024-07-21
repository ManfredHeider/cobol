      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1129.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1129.CPv  $
      *
      *   Rev 2.1   Mar 11 2016 10:53:22   t13ex71
      *ARW00109: Anpassungen im PMRW0017 (BPR)
      *
      *   Rev 2.0   Mar 08 2016 14:42:40   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1129                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0049                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: ZGUR-VUJIC                                       *
      *    DATUM:    29.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00109!08.03.16! 01 ! NEUERSTELLUNG             !B.Pretsch *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1129                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1129.
          05 PI-DRVB1129-PROGRAMM                PIC  X(008).
          05 PI-DRVB1129-MA-ST-NR                PIC  9(007) COMP-3.
      *
       01 PO-DRVB1129.
 
          05 PO-DRVB1129-RC                      PIC  X(003).
             88 PO-DRVB1129-RC-OK                        VALUE 'OK '.
 
          05 PO-DRVB1129-DATEN.
             10 PO-DRVB1129-MAT-DAT-BIS          PIC  X(010).
             10 PO-DRVB1129-MAT-EINTR-KUEND      PIC  X(010).
      *
      ***** Ende Copy-Member DRVB1129 *****************************

