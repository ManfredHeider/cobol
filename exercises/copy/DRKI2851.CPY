      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2851.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2851.CPv  $
      *
      *   Rev 2.6   Nov 04 2014 08:56:24   t13ex78
      *AKI01324: Erweiterung der Tabelle KUNDENLOESCHUNG - OZP's an
      *passen
      *
      *   Rev 2.5   Oct 29 2014 11:00:34   t13ex78
      *AKI01324: Erweiterung der Tabelle KUNDENLOESCHUNG - OZP's an
      *passen
      *
      *   Rev 2.4   May 16 2014 13:09:38   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden -
      *ZTKI0000
      *
      *   Rev 2.3   May 15 2014 11:55:26   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden -
      *Neugenerien des ZTKI0000 wegen NULL-Indikator
      *
      *   Rev 2.2   Apr 22 2014 11:44:06   t13ex78
      *AKI01063: Wartungsauftrag-Ticketverarbeitung Kundenlöschung
      *Gemeinschaftskonten
      *
      *   Rev 2.1   Mar 27 2014 11:27:22   t13ex78
      *AKI01063: Wartungsauftrag-Ticketverarbeitung - Kundenlöschun
      *g Gemeinschaftskonten
      *
      *   Rev 2.0   Mar 17 2014 10:48:13 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2851                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2851                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         29.10.2014 / 09:36:46 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2851
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2851.
           05  PI-DRKI2851-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2851-KULPERSONENID   PIC  X(0013).
             10 PI-DRKI2851-KULVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2851.
           05  PO-DRKI2851-RC                     PIC  X(003).
               88 PO-DRKI2851-RC-OK               VALUE 'OK '.
               88 PO-DRKI2851-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2851-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2851-KULERSTELLTTST   PIC  X(026).
             10 PO-DRKI2851-KULKNDNUMMER   PIC S9(9) COMP.
             10 PO-DRKI2851-KULMANDANTNR   PIC S9(9) COMP.
             10 PO-DRKI2851-KULPARTGESELLSCH   PIC S9(4) COMP.
             10 PO-DRKI2851-KULSTATUS   PIC  X(0001).
             10 PO-DRKI2851-KULTICKETID   PIC S9(18)V COMP-3.
             10 PO-DRKI2851-KULVBNR   PIC S9(9) COMP.
             10 PO-DRKI2851-KULVERTRAGNUMMER   PIC  X(0015).
             10 PO-DRKI2851-LETZTEAENDERUNG   PIC  X(026).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
