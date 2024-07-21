      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2870.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2870.CPv  $
      *
      *   Rev 2.3   Mar 18 2015 14:54:42   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Multirow-OZP 
      *für TCKI0418
      *
      *   Rev 2.2   Oct 08 2014 13:40:32   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:02:14   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:25:37 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2870                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2870                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.02.2015 / 12:09:19 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2870
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2870.
          05 PI-DRKI2870-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2870-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI2870-KEY.
            10 PI-DRKI2870-VERVERTRAGSNUMMER      PIC  X(015).
            10 PI-DRKI2870-VERPARTGESELLSCHID PIC S9(4) USAGE COMP.
 
      *
       01  PO-DRKI2870.
           05  PO-DRKI2870-RC                     PIC  X(003).
             88 PO-DRKI2870-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2870-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2870-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2870-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2870-DATEN.
 
           10 PO-DRKI2870-VGVVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVRANSCHRIFTPLZ PIC  X(0006)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVRANSCHRSTR PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVRANSCHRHAUSNR PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVRANSCHRIFTORT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVRANSCHRIFTLKZ PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVBEDINGUNG PIC  X(0016)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVRISIKO PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVKLAUSELTARIF PIC  X(0080)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVBEILAGEN PIC  X(0019)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVZUSATZINFO PIC  X(0100)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVFEUERKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVLEITUNGSWASKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVSTURMKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVHAGELKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVELEMSCHLKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVGEBAUENUTZUNG PIC  X(0026)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVGEWERBFLPROZ PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVBAUJAHR PIC S9(8)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2870-VGVBAUARTKL-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVBAUARTKL-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2870-VGVROHBAUBISDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2870-NI-VGVROHBAUBISDAT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVLEISARTLB-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVLEISARTLB-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2870-VGVUNTERVERSVERZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVGRUNDFUERUVZ PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVVERSSUM1914 PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVSTAFFPRAEMKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVZLEITWASSER PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVZSTURM PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVZERDBEBEN PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVZELEMENTSCH PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVMINDBEITRAGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVLEISTFREIVDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2870-N-VGVLSTFRVDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVLEISTFREIBDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2870-N-VGVLSTFRBDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVSELBSTBTEXT PIC  X(0014)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVPRMSATZWAGNIS PIC S9(2)V9(7) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2870-VGVPRMSATZGEFAHR PIC S9(2)V9(4) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2870-VGVANZVSCHGEFAHR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVGEFKLASSE PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVGKLASFLAEPROZ PIC  X(0020)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVPRODUKT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVGEBAUEDEART PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVBETRIEBSARTLB PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVMODELL PIC  X(0018)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVMONOPOLKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVAUSLRISIKOKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVGEBBEZ PIC  X(0080)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVROHBAUVER-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVROHBAUVER-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2870-VGVWOHNFLAECHE PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVANZGESCHOSSE PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVKELLERVORHKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVSCHWBADGEBKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVNGEBENTSCHGRZ PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVNEBENGEBAEUDE PIC  X(0080)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVSONSTIGES PIC  X(0080)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVGARAGENCARPOR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVZUEBERSCHWEM PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVMAILINGAKTION PIC  X(0070)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVDIFFERDECKKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVUMSTELLDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2870-NI-VGVUMSTELLDAT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2870-VGVGESELLSCHAFT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVVERSNUMMER PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVKUENDIGUNGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVELEDIFVERSKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2870-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.
           10 PO-DRKI2870-VGVSNUTZFLAECHE PIC S9(4) COMP
                                   OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************

