      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3291.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3291.CPv  $
      *
      *   Rev 2.2   Dec 15 2020 15:12:24   t13ae31
      *AKI01812: G21 Leben Basis Viridium DB2
      *
      *   Rev 2.1   Apr 13 2018 10:14:12   t13ex19
      *AKI01570: FW - Generali-Partnergesellschaften - Basisdaten-V
      *ertrag
      *
      *   Rev 2.0   Feb 22 2018 17:43:12 T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3291                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3291                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.02.2018 / 17:43:12 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI3291
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3291.
           05  PI-DRKI3291-UFKT-CODE              PIC 9(002).
               88 PI-DRKI3291-UFKT-FIRST          VALUE 1.
               88 PI-DRKI3291-UFKT-NEXT           VALUE 2.
               88 PI-DRKI3291-UFKT-QUIT           VALUE 3.
      *
       01  PO-DRKI3291.
           05  PO-DRKI3291-RC                     PIC  X(003).
               88 PO-DRKI3291-RC-OK               VALUE 'OK '.
               88 PO-DRKI3291-RC-LEER             VALUE 'LER'.
               88 PO-DRKI3291-RC-EOF              VALUE 'EOF'.

      *
           05  PO-DRKI3291-DATEN.
             10 PO-DRKI3291-GENVGESELLSCHID  PIC X(002).
             10 PO-DRKI3291-GENVVNR          PIC X(015).
             10 PO-DRKI3291-GENVEZPNR        PIC S9(4) COMP.
             10 PO-DRKI3291-GENVABLAUF   PIC  X(0008).
             10 PO-DRKI3291-GENVAENDKZ   PIC  X(0001).
             10 PO-DRKI3291-GENVAKTWERT   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3291-GENVAMTLKENNZ   PIC  X(0012).
             10 PO-DRKI3291-GENVBEGINN   PIC  X(0008).
             10 PO-DRKI3291-GENVBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3291-GENVFLAECHE   PIC S9(4) COMP.
             10 PO-DRKI3291-GENVGARRENTE   PIC S9(6)V COMP-3.
             10 PO-DRKI3291-GENVGUTHABEN   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3291-GENVGZWEIG   PIC  X(0002).
             10 PO-DRKI3291-GENVNETTOBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3291-GENVPRNUMMER   PIC S9(9) COMP.
             10 PO-DRKI3291-GENVPRODUKT   PIC  X(0050).
             10 PO-DRKI3291-GENVRENTENBEZUG   PIC  X(0001).
             10 PO-DRKI3291-GENVSPARTE   PIC  X(0003).
             10 PO-DRKI3291-GENVSTAND   PIC  X(0008).
             10 PO-DRKI3291-GENVSUMME   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3291-GENVTARIF   PIC  X(0050).
             10 PO-DRKI3291-GENVVBNR   PIC S9(9) COMP.
             10 PO-DRKI3291-GENVVERARB-KZN   PIC  X(0001).
             10 PO-DRKI3291-GENVVORRENTE   PIC S9(6)V COMP-3.
             10 PO-DRKI3291-GENVWAGNISKZ   PIC S9(4) COMP.
             10 PO-DRKI3291-GENVZAHLWEISE   PIC  X(0002).
             10 PO-DRKI3291-GENVZUSTAND   PIC S9(4) COMP.
             10 PO-DRKI3291-GENVMIGKZN    PIC X(01).
      *
      ******* ENDE COPY-MEMBER ***************************************


