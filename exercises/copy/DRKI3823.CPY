      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3823.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3823.CPv  $
      *
      *   Rev 2.2   Jul 31 2020 13:59:02   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.1   Jul 01 2020 09:30:22   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:02:39 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3823                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3823                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.07.2020 / 13:32:51 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3823
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3823.
           05  PI-DRKI3823-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3823-AKPANGEBOTSNAME   PIC  X(0010).
             10 PI-DRKI3823-AKPPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3823-AKPVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI3823-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3823-NI-AKPANZ-ANGEBOTE  PIC S9(4) COMP.
             15 PI-DRKI3823-NI-AKPGEBDAT-VP     PIC S9(4) COMP.
             15 PI-DRKI3823-NI-AKPNACHNAME-VP   PIC S9(4) COMP.
             15 PI-DRKI3823-NI-AKPUMST-TERMIN   PIC S9(4) COMP.
             15 PI-DRKI3823-NI-AKPVORNAME-VP    PIC S9(4) COMP.
             15 PI-DRKI3823-NI-LETZTEAENDERUNG  PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3823-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3823-AKPANZ-ANGEBOTE   PIC S9(4) COMP.
             10 PI-DRKI3823-AKPGEBDAT-VP   PIC  X(010).
             10 PI-DRKI3823-AKPNACHNAME-VP   PIC  X(0040).
             10 PI-DRKI3823-AKPUMST-TERMIN   PIC  X(010).
             10 PI-DRKI3823-AKPVORNAME-VP   PIC  X(0030).
             10 PI-DRKI3823-LETZTEAENDERUNG   PIC  X(0026).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3823-NI1.
           05 PI-DRKI3823-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 6.
       01  PI-DRKI3823-NI7.
           05 PI-DRKI3823-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 6.
      *
       01  PO-DRKI3823.
           05  PO-DRKI3823-RC                     PIC  X(003).
               88 PO-DRKI3823-RC-OK               VALUE 'OK '.
               88 PO-DRKI3823-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

