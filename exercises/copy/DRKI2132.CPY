      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2132.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2132.CPv  $
      *
      *   Rev 2.2   Mar 18 2015 15:08:24   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0418
      *
      *   Rev 2.1   Feb 27 2008 20:03:18   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 13:40:58   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2132                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2132                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.01.2015 / 09:55:21 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2132
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2132.
           05  PI-DRKI2132-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2132-VGVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2132-VGVVETLFDNR   PIC S9(4) COMP.
END   *
 
      *
       01  PO-DRKI2132.
           05  PO-DRKI2132-RC                     PIC  X(003).
               88 PO-DRKI2132-RC-OK               VALUE 'OK '.
               88 PO-DRKI2132-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2132-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2132-LETZTEAENDERUNG   PIC  X(026).
             10 PO-DRKI2132-VGVANZGESCHOSSE   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVANZVSCHGEFAHR   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVAUSLRISIKOKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVBAUARTKL-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2132-VGVBAUARTKL-KTY   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVBAUJAHR   PIC S9(8)V COMP-3.
             10 PO-DRKI2132-VGVBEDINGUNG   PIC  X(0016).
             10 PO-DRKI2132-VGVBEILAGEN   PIC  X(0019).
             10 PO-DRKI2132-VGVBETRIEBSARTLB   PIC  X(0050).
             10 PO-DRKI2132-VGVDIFFERDECKKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVEINZVTGHOSTID   PIC  X(0015).
             10 PO-DRKI2132-VGVELEDIFVERSKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVELEMSCHLKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVFEUERKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVGARAGENCARPOR   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVGEBAUEDEART   PIC  X(0050).
             10 PO-DRKI2132-VGVGEBAUENUTZUNG   PIC  X(0026).
             10 PO-DRKI2132-VGVGEBBEZ   PIC  X(0080).
             10 PO-DRKI2132-VGVGEFKLASSE   PIC  X(0001).
             10 PO-DRKI2132-VGVGESELLSCHAFT   PIC  X(0050).
             10 PO-DRKI2132-VGVGEWERBFLPROZ   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVGKLASFLAEPROZ   PIC  X(0020).
             10 PO-DRKI2132-VGVGRUNDFUERUVZ   PIC  X(0040).
             10 PO-DRKI2132-VGVHAGELKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVKELLERVORHKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVKLAUSELTARIF   PIC  X(0080).
             10 PO-DRKI2132-VGVKUENDIGUNGKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVLEISARTLB-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2132-VGVLEISARTLB-KTY   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVLEISTFREIBDAT   PIC  X(010).
             10 PO-DRKI2132-VGVLEISTFREIVDAT   PIC  X(010).
             10 PO-DRKI2132-VGVLEITUNGSWASKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVMAILINGAKTION   PIC  X(0070).
             10 PO-DRKI2132-VGVMINDBEITRAGKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVMODELL   PIC  X(0018).
             10 PO-DRKI2132-VGVMONOPOLKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVNEBENGEBAEUDE   PIC  X(0080).
             10 PO-DRKI2132-VGVNGEBENTSCHGRZ   PIC S9(9) COMP.
             10 PO-DRKI2132-VGVPRMSATZGEFAHR   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2132-VGVPRMSATZWAGNIS   PIC S9(2)V9(7) COMP-3.
             10 PO-DRKI2132-VGVPRODUKT   PIC  X(0030).
             10 PO-DRKI2132-VGVRANSCHRHAUSNR   PIC  X(0015).
             10 PO-DRKI2132-VGVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PO-DRKI2132-VGVRANSCHRIFTORT   PIC  X(0030).
             10 PO-DRKI2132-VGVRANSCHRIFTPLZ   PIC  X(0006).
             10 PO-DRKI2132-VGVRANSCHRSTR   PIC  X(0040).
             10 PO-DRKI2132-VGVRISIKO   PIC  X(0050).
             10 PO-DRKI2132-VGVROHBAUBISDAT   PIC  X(010).
             10 PO-DRKI2132-VGVROHBAUVER-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2132-VGVROHBAUVER-KTY   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVSCHWBADGEBKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVSELBSTBTEXT   PIC  X(0014).
             10 PO-DRKI2132-VGVSNUTZFLAECHE   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVSONSTIGES   PIC  X(0080).
             10 PO-DRKI2132-VGVSTAFFPRAEMKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVSTURMKZ   PIC  X(0002).
             10 PO-DRKI2132-VGVUMSTELLDAT   PIC  X(010).
             10 PO-DRKI2132-VGVUNTERVERSVERZ   PIC  X(0002).
             10 PO-DRKI2132-VGVVERSNUMMER   PIC  X(0040).
             10 PO-DRKI2132-VGVVERSSUM1914   PIC S9(9) COMP.
             10 PO-DRKI2132-VGVWOHNFLAECHE   PIC S9(9) COMP.
             10 PO-DRKI2132-VGVZELEMENTSCH   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVZERDBEBEN   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVZLEITWASSER   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVZSTURM   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVZUEBERSCHWEM   PIC S9(4) COMP.
             10 PO-DRKI2132-VGVZUSATZINFO   PIC  X(0100).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

