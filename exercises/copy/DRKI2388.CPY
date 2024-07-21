      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2388.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2388.CPv  $
      *
      *   Rev 2.2   Sep 06 2012 14:28:06   t13ae49
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.1   Sep 06 2012 10:58:08   t13ex14
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.0   Mar 22 2010 07:41:40   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2388                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2388                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         03.11.2011 / 18:47:24 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2388
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2388.
           05  PI-DRKI2388-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2388-KPEPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2388-KPEVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2388-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2388-KPEADRESSEID   PIC  X(0013).
             10 PI-DRKI2388-KPEANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2388-KPEBEGINNDATUM   PIC  X(010).
             10 PI-DRKI2388-KPEENDEDATUM   PIC  X(010).
             10 PI-DRKI2388-KPEGEBDATUM   PIC  X(010).
             10 PI-DRKI2388-KPEGESCHSTID-KNR   PIC S9(4) COMP.
             10 PI-DRKI2388-KPENACHNAME   PIC  X(0030).
             10 PI-DRKI2388-KPEPERSONENID   PIC  X(0013).
             10 PI-DRKI2388-KPEPERSONENNUMMER   PIC S9(4) COMP.
             10 PI-DRKI2388-KPEPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2388-KPEVORNAME   PIC  X(0030).
END   *

      *
       01  PO-DRKI2388.
           05  PO-DRKI2388-RC                     PIC  X(003).
               88 PO-DRKI2388-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2388-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
