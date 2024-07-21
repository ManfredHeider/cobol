      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2609.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2609.CPv  $
      *
      *   Rev 2.3   May 16 2014 13:09:58   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden - 
      *ZTKI0000
      *
      *   Rev 2.2   May 15 2014 11:55:48   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden -
      *Neugenerien des ZTKI0000 wegen NULL-Indikator
      *
      *   Rev 2.1   Jul 20 2012 09:54:40   t13ex27
      *AKI01135: Kundendaten physisch löschen-OZPs und NKDA-Copys
      *
      *   Rev 2.0   May 8 2012 11:18:06 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2609                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2609                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.05.2014 / 11:15:52 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2609
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2609.
           05  PI-DRKI2609-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2609-KULPERSONENID   PIC  X(0013).
             10 PI-DRKI2609-KULVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2609-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2609-KULSTATUS   PIC  X(0001).
END   *


      *
       01  PO-DRKI2609.
           05  PO-DRKI2609-RC                     PIC  X(003).
               88 PO-DRKI2609-RC-OK               VALUE 'OK '.
               88 PO-DRKI2609-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
