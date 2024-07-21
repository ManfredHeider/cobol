      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.08.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2658.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2658.CPv  $
      *
      *   Rev 2.2   Oct 18 2012 10:26:02   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Sep 26 2012 10:12:30   t13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Aug 22 2012 16:59:30 T13EX52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2658                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2658                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.10.2012 / 18:24:37 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2658
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2658.
           05  PI-DRKI2658-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2658-VVSVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2658-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2658-VVSKUNDBINDVTRG   PIC  X(0001).
             10 PI-DRKI2658-VVSSTORNOHAFTUNG   PIC  X(0001).
             10 PI-DRKI2658-VVSVERTRAGSNUMMER   PIC  X(0015).
             10 PI-DRKI2658-VVSVTRGBEITRRUEK   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2658-VVSVTRGERHOEHPRZ   PIC S9(7)V9(2) COMP-3.
END   *

      *
       01  PO-DRKI2658.
           05  PO-DRKI2658-RC                     PIC  X(003).
               88 PO-DRKI2658-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2658-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
