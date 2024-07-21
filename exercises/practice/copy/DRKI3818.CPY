      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3818.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3818.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:27:02   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 14:45:04 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3818                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3818                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         24.06.2020 / 14:45:04 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3818
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3818.
           05  PI-DRKI3818-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3818-AKVANGEBOTSNAME   PIC  X(0010).
             10 PI-DRKI3818-AKVVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI3818-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3818-NI-AKVANNAHME-BIS   PIC S9(4) COMP.
             15 PI-DRKI3818-NI-AKVANZ-VP        PIC S9(4) COMP.
             15 PI-DRKI3818-N-KVFBWHR-BS        PIC S9(4) COMP.
             15 PI-DRKI3818-NI-AKVNACHNAME-VN   PIC S9(4) COMP.
             15 PI-DRKI3818-NI-AKVUMST-TERMIN   PIC S9(4) COMP.
             15 PI-DRKI3818-N-KVVRTRGSNMMR      PIC S9(4) COMP.
             15 PI-DRKI3818-NI-AKVVORNAME-VN    PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3818-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3818-AKVANNAHME-BIS   PIC  X(010).
             10 PI-DRKI3818-AKVANZ-VP   PIC S9(4) COMP.
             10 PI-DRKI3818-AKVAUFBEWAHR-BIS   PIC  X(010).
             10 PI-DRKI3818-AKVNACHNAME-VN   PIC  X(0040).
             10 PI-DRKI3818-AKVUMST-TERMIN   PIC  X(010).
             10 PI-DRKI3818-AKVVERTRAGSNUMMER   PIC  X(0015).
             10 PI-DRKI3818-AKVVORNAME-VN   PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3818-NI1.
           05 PI-DRKI3818-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 7.
       01  PI-DRKI3818-NI7.
           05 PI-DRKI3818-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 7.
      *
       01  PO-DRKI3818.
           05  PO-DRKI3818-RC                     PIC  X(003).
               88 PO-DRKI3818-RC-OK               VALUE 'OK '.
               88 PO-DRKI3818-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

