      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3816.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3816.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:25:42   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 14:45:00 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3816                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3816                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         24.06.2020 / 14:45:00 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3816
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3816.
           05  PI-DRKI3816-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3816-AKVANGEBOTSNAME   PIC  X(0010).
             10 PI-DRKI3816-AKVVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3816.
           05  PO-DRKI3816-RC                     PIC  X(003).
               88 PO-DRKI3816-RC-OK               VALUE 'OK '.
               88 PO-DRKI3816-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3816-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3816-AKVANNAHME-BIS   PIC  X(010).
             10 PO-DRKI3816-AKVANZ-VP   PIC S9(4) COMP.
             10 PO-DRKI3816-AKVAUFBEWAHR-BIS   PIC  X(010).
             10 PO-DRKI3816-AKVNACHNAME-VN   PIC  X(0040).
             10 PO-DRKI3816-AKVUMST-TERMIN   PIC  X(010).
             10 PO-DRKI3816-AKVVERTRAGSNUMMER   PIC  X(0015).
             10 PO-DRKI3816-AKVVORNAME-VN   PIC  X(0030).
             10 PO-DRKI3816-LETZTEAENDERUNG   PIC  X(0026).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

