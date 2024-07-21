      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3828.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3828.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 09:33:46   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source Db2
      *
      *   Rev 2.0   Jun 24 2020 15:04:46 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3828                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3828                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         24.06.2020 / 15:04:46 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3828
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3828.
           05  PI-DRKI3828-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3828-AKOANGEBOTSNAME   PIC  X(0010).
             10 PI-DRKI3828-AKOPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3828-AKOTARIFTEIL-ID   PIC  X(0010).
             10 PI-DRKI3828-AKOVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI3828-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3828-N-KGSMT-BTRG        PIC S9(4) COMP.
             15 PI-DRKI3828-N-KGSTZL-ZSCHL      PIC S9(4) COMP.
             15 PI-DRKI3828-N-KPSCH-LST-1       PIC S9(4) COMP.
             15 PI-DRKI3828-N-KPSCH-LST-2       PIC S9(4) COMP.
             15 PI-DRKI3828-N-KPSCH-LST-3       PIC S9(4) COMP.
             15 PI-DRKI3828-N-KPSCH-LST-4       PIC S9(4) COMP.
             15 PI-DRKI3828-NI-AKOSELBSTBEHALT  PIC S9(4) COMP.
             15 PI-DRKI3828-NI-AKOSTICHTAG      PIC S9(4) COMP.
             15 PI-DRKI3828-N-KSM-BTRZSCHL      PIC S9(4) COMP.
             15 PI-DRKI3828-NI-AKOTARIFNAME     PIC S9(4) COMP.
             15 PI-DRKI3828-N-KVRS-LSTNG        PIC S9(4) COMP.
             15 PI-DRKI3828-N-KZTL-BFR-LM       PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3828-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3828-AKOGESAMT-BEITRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3828-AKOGESETZL-ZUSCHL   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3828-AKOPAUSCH-LEIST-1   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3828-AKOPAUSCH-LEIST-2   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3828-AKOPAUSCH-LEIST-3   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3828-AKOPAUSCH-LEIST-4   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3828-AKOSELBSTBEHALT   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3828-AKOSTICHTAG   PIC  X(010).
             10 PI-DRKI3828-AKOSUM-BEITRZUSCHL   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3828-AKOTARIFNAME   PIC  X(0040).
             10 PI-DRKI3828-AKOVERS-LEISTUNG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3828-AKOZEITL-BEFR-LIM   PIC S9(7)V9(2) COMP-3.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3828-NI1.
           05 PI-DRKI3828-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 12.
       01  PI-DRKI3828-NI7.
           05 PI-DRKI3828-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 12.
      *
       01  PO-DRKI3828.
           05  PO-DRKI3828-RC                     PIC  X(003).
               88 PO-DRKI3828-RC-OK               VALUE 'OK '.
               88 PO-DRKI3828-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

