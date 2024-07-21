      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3821.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3821.CPv  $
      *
      *   Rev 2.2   Jul 31 2020 13:57:36   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.1   Jul 01 2020 09:29:02   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:02:36 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3821                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3821                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.07.2020 / 13:32:48 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3821
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3821.
           05  PI-DRKI3821-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3821-AKPANGEBOTSNAME   PIC  X(0010).
             10 PI-DRKI3821-AKPPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3821-AKPVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3821.
           05  PO-DRKI3821-RC                     PIC  X(003).
               88 PO-DRKI3821-RC-OK               VALUE 'OK '.
               88 PO-DRKI3821-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3821-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3821-AKPANZ-ANGEBOTE   PIC S9(4) COMP.
             10 PO-DRKI3821-AKPGEBDAT-VP   PIC  X(010).
             10 PO-DRKI3821-AKPNACHNAME-VP   PIC  X(0040).
             10 PO-DRKI3821-AKPUMST-TERMIN   PIC  X(010).
             10 PO-DRKI3821-AKPVORNAME-VP   PIC  X(0030).
             10 PO-DRKI3821-LETZTEAENDERUNG   PIC  X(0026).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

