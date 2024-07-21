      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.10.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0758.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI0758.CPv  $
      *
      *   Rev 2.6   Jan 11 2012 09:08:32   t13ex14
      *AKI01079: normalisieren VERTRAGSARTBEZ; DB-Objekte
      *
      *   Rev 2.5   Nov 11 2011 14:22:08   t13ex14
      *AKI01079: DB-Objekte: TBAT0039 / VERTRAGSARTBEZ
      *
      *   Rev 2.0   Oct 7 2011 13:57:12 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0758                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0758                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.10.2011 / 13:57:12 / T13EX14
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
      *    FÜR DIE FUNKTION DRKI0758
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0758.
           05  PI-DRKI0758-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI0758-VRTLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0758-VRTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0758-VRTVERTRAGSARTID   PIC S9(4) COMP.
             10 PI-DRKI0758-VRTVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI0758-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI0758-VRTVERTART-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0758-VRTVERTART-KTY   PIC S9(4) COMP.
             10 PI-DRKI0758-VRTVERTRAGSART.
                49 PI-DRKI0758-VRTVERTRAGSART-LEN PIC S9(4) COMP.
                49 PI-DRKI0758-VRTVRTRGSRT-TEXT PIC X(50).
             10 PI-DRKI0758-VRTVERTRAGWAGNISKZ   PIC  X(0001).
END   *

      *
       01  PO-DRKI0758.
           05  PO-DRKI0758-RC                     PIC  X(003).
               88 PO-DRKI0758-RC-OK                  VALUE 'OK '.
               88 PO-DRKI0758-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
