      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3826.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3826.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:32:26   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:04:41 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3826                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3826                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         24.06.2020 / 15:04:41 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3826
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3826.
           05  PI-DRKI3826-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3826-AKOANGEBOTSNAME   PIC  X(0010).
             10 PI-DRKI3826-AKOPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3826-AKOTARIFTEIL-ID   PIC  X(0010).
             10 PI-DRKI3826-AKOVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3826.
           05  PO-DRKI3826-RC                     PIC  X(003).
               88 PO-DRKI3826-RC-OK               VALUE 'OK '.
               88 PO-DRKI3826-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3826-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3826-AKOGESAMT-BEITRAG   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3826-AKOGESETZL-ZUSCHL   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3826-AKOPAUSCH-LEIST-1   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3826-AKOPAUSCH-LEIST-2   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3826-AKOPAUSCH-LEIST-3   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3826-AKOPAUSCH-LEIST-4   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3826-AKOSELBSTBEHALT   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3826-AKOSTICHTAG   PIC  X(010).
             10 PO-DRKI3826-AKOSUM-BEITRZUSCHL   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3826-AKOTARIFNAME   PIC  X(0040).
             10 PO-DRKI3826-AKOVERS-LEISTUNG   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3826-AKOZEITL-BEFR-LIM   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3826-LETZTEAENDERUNG   PIC  X(0026).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

