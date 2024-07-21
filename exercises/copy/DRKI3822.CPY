      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3822.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3822.CPv  $
      *
      *   Rev 2.2   Jul 31 2020 13:58:20   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.1   Jul 01 2020 09:29:42   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:02:37 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3822                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3822                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.07.2020 / 13:32:49 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3822
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3822.
           05  PI-DRKI3822-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3822-AKPANGEBOTSNAME   PIC  X(0010).
             10 PI-DRKI3822-AKPPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3822-AKPVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI3822-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3822-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3822-AKPANZ-ANGEBOTE   PIC S9(4) COMP.
             10 PI-DRKI3822-AKPGEBDAT-VP   PIC  X(010).
             10 PI-DRKI3822-AKPNACHNAME-VP   PIC  X(0040).
             10 PI-DRKI3822-AKPUMST-TERMIN   PIC  X(010).
             10 PI-DRKI3822-AKPVORNAME-VP   PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3822-NI1.
           05 PI-DRKI3822-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3822-NI7.
           05 PI-DRKI3822-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3822.
           05  PO-DRKI3822-RC                     PIC  X(003).
               88 PO-DRKI3822-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3822-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

