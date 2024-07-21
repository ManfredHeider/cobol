      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2608.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2608.CPv  $
      *
      *   Rev 2.5   Nov 04 2014 08:56:18   t13ex78
      *AKI01324: Erweiterung der Tabelle KUNDENLOESCHUNG - OZP's an
      *passen
      *
      *   Rev 2.4   Oct 29 2014 11:00:28   t13ex78
      *AKI01324: Erweiterung der Tabelle KUNDENLOESCHUNG - OZP's an
      *passen
      *
      *   Rev 2.3   May 16 2014 13:09:52   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden -
      *ZTKI0000
      *
      *   Rev 2.2   May 15 2014 11:55:40   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden -
      *Neugenerien des ZTKI0000 wegen NULL-Indikator
      *
      *   Rev 2.1   Jul 20 2012 09:54:32   t13ex27
      *AKI01135: Kundendaten physisch löschen-OZPs und NKDA-Copys
      *
      *   Rev 2.0   May 8 2012 11:18:01 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2608                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2608                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         29.10.2014 / 09:36:42 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2608
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2608.
           05  PI-DRKI2608-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2608-KULPERSONENID   PIC  X(0013).
             10 PI-DRKI2608-KULVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2608-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2608-KULKNDNUMMER   PIC S9(9) COMP.
             10 PI-DRKI2608-KULMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI2608-KULPARTGESELLSCH   PIC S9(4) COMP.
             10 PI-DRKI2608-KULSTATUS   PIC  X(0001).
             10 PI-DRKI2608-KULTICKETID   PIC S9(18)V COMP-3.
             10 PI-DRKI2608-KULVBNR   PIC S9(9) COMP.
             10 PI-DRKI2608-KULVERTRAGNUMMER   PIC  X(0015).
END   *

      *
       01  PO-DRKI2608.
           05  PO-DRKI2608-RC                     PIC  X(003).
               88 PO-DRKI2608-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2608-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
