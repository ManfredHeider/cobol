      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2130.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2130.CPv  $
      *
      *   Rev 2.2   Mar 18 2015 15:08:14   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0418
      *
      *   Rev 2.1   Feb 27 2008 20:03:04   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 13:34:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2130                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2130                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.01.2015 / 09:55:16 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2130
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2130.
           05  PI-DRKI2130-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2130-VGVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2130-VGVVETLFDNR   PIC S9(4) COMP.
END   *
 
 
           05  PI-DRKI2130-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2130-LETZTEAENDERUNG   PIC  X(026).
             10 PI-DRKI2130-VGVANZGESCHOSSE   PIC S9(4) COMP.
             10 PI-DRKI2130-VGVANZVSCHGEFAHR   PIC S9(4) COMP.
             10 PI-DRKI2130-VGVAUSLRISIKOKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVBAUARTKL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2130-VGVBAUJAHR   PIC S9(8)V COMP-3.
             10 PI-DRKI2130-VGVBEDINGUNG   PIC  X(0016).
             10 PI-DRKI2130-VGVBEILAGEN   PIC  X(0019).
             10 PI-DRKI2130-VGVBETRIEBSARTLB   PIC  X(0050).
             10 PI-DRKI2130-VGVDIFFERDECKKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI2130-VGVELEDIFVERSKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVELEMSCHLKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVFEUERKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVGARAGENCARPOR   PIC S9(4) COMP.
             10 PI-DRKI2130-VGVGEBAUEDEART   PIC  X(0050).
             10 PI-DRKI2130-VGVGEBAUENUTZUNG   PIC  X(0026).
             10 PI-DRKI2130-VGVGEBBEZ   PIC  X(0080).
             10 PI-DRKI2130-VGVGEFKLASSE   PIC  X(0001).
             10 PI-DRKI2130-VGVGESELLSCHAFT   PIC  X(0050).
             10 PI-DRKI2130-VGVGEWERBFLPROZ   PIC S9(4) COMP.
             10 PI-DRKI2130-VGVGKLASFLAEPROZ   PIC  X(0020).
             10 PI-DRKI2130-VGVGRUNDFUERUVZ   PIC  X(0040).
             10 PI-DRKI2130-VGVHAGELKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVKELLERVORHKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVKLAUSELTARIF   PIC  X(0080).
             10 PI-DRKI2130-VGVKUENDIGUNGKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVLEISARTLB-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2130-VGVLEISTFREIBDAT   PIC  X(010).
             10 PI-DRKI2130-VGVLEISTFREIVDAT   PIC  X(010).
             10 PI-DRKI2130-VGVLEITUNGSWASKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVMAILINGAKTION   PIC  X(0070).
             10 PI-DRKI2130-VGVMINDBEITRAGKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVMODELL   PIC  X(0018).
             10 PI-DRKI2130-VGVMONOPOLKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVNEBENGEBAEUDE   PIC  X(0080).
             10 PI-DRKI2130-VGVNGEBENTSCHGRZ   PIC S9(9) COMP.
             10 PI-DRKI2130-VGVPRMSATZGEFAHR   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2130-VGVPRMSATZWAGNIS   PIC S9(2)V9(7) COMP-3.
             10 PI-DRKI2130-VGVPRODUKT   PIC  X(0030).
             10 PI-DRKI2130-VGVRANSCHRHAUSNR   PIC  X(0015).
             10 PI-DRKI2130-VGVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PI-DRKI2130-VGVRANSCHRIFTORT   PIC  X(0030).
             10 PI-DRKI2130-VGVRANSCHRIFTPLZ   PIC  X(0006).
             10 PI-DRKI2130-VGVRANSCHRSTR   PIC  X(0040).
             10 PI-DRKI2130-VGVRISIKO   PIC  X(0050).
             10 PI-DRKI2130-VGVROHBAUBISDAT   PIC  X(010).
             10 PI-DRKI2130-VGVROHBAUVER-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2130-VGVSCHWBADGEBKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVSELBSTBTEXT   PIC  X(0014).
             10 PI-DRKI2130-VGVSNUTZFLAECHE   PIC S9(4) COMP.
             10 PI-DRKI2130-VGVSONSTIGES   PIC  X(0080).
             10 PI-DRKI2130-VGVSTAFFPRAEMKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVSTURMKZ   PIC  X(0002).
             10 PI-DRKI2130-VGVUMSTELLDAT   PIC  X(010).
             10 PI-DRKI2130-VGVUNTERVERSVERZ   PIC  X(0002).
             10 PI-DRKI2130-VGVVERSNUMMER   PIC  X(0040).
             10 PI-DRKI2130-VGVVERSSUM1914   PIC S9(9) COMP.
             10 PI-DRKI2130-VGVWOHNFLAECHE   PIC S9(9) COMP.
             10 PI-DRKI2130-VGVZELEMENTSCH   PIC S9(4) COMP.
             10 PI-DRKI2130-VGVZERDBEBEN   PIC S9(4) COMP.
             10 PI-DRKI2130-VGVZLEITWASSER   PIC S9(4) COMP.
             10 PI-DRKI2130-VGVZSTURM   PIC S9(4) COMP.
             10 PI-DRKI2130-VGVZUEBERSCHWEM   PIC S9(4) COMP.
             10 PI-DRKI2130-VGVZUSATZINFO   PIC  X(0100).
END   *
 
 
      *
       01  PO-DRKI2130.
           05  PO-DRKI2130-RC                     PIC  X(003).
               88 PO-DRKI2130-RC-OK               VALUE 'OK '.
               88 PO-DRKI2130-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

