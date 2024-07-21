      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2129.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2129.CPv  $
      *
      *   Rev 2.2   Mar 18 2015 15:08:10   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0418
      *
      *   Rev 2.1   Feb 27 2008 20:02:56   t13ex51
      *AKL00665: Allfinanz; OZPs Privatkunden
      *
      *   Rev 2.0   28 Sep 2007 13:31:40   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2129                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2129                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.01.2015 / 09:55:13 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2129
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2129.
           05  PI-DRKI2129-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2129-VGVVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2129-VGVVETLFDNR   PIC S9(4) COMP.
END   *
 
 
           05  PI-DRKI2129-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2129-LETZTEAENDERUNG   PIC  X(026).
             10 PI-DRKI2129-VGVANZGESCHOSSE   PIC S9(4) COMP.
             10 PI-DRKI2129-VGVANZVSCHGEFAHR   PIC S9(4) COMP.
             10 PI-DRKI2129-VGVAUSLRISIKOKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVBAUARTKL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2129-VGVBAUJAHR   PIC S9(8)V COMP-3.
             10 PI-DRKI2129-VGVBEDINGUNG   PIC  X(0016).
             10 PI-DRKI2129-VGVBEILAGEN   PIC  X(0019).
             10 PI-DRKI2129-VGVBETRIEBSARTLB   PIC  X(0050).
             10 PI-DRKI2129-VGVDIFFERDECKKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI2129-VGVELEDIFVERSKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVELEMSCHLKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVFEUERKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVGARAGENCARPOR   PIC S9(4) COMP.
             10 PI-DRKI2129-VGVGEBAUEDEART   PIC  X(0050).
             10 PI-DRKI2129-VGVGEBAUENUTZUNG   PIC  X(0026).
             10 PI-DRKI2129-VGVGEBBEZ   PIC  X(0080).
             10 PI-DRKI2129-VGVGEFKLASSE   PIC  X(0001).
             10 PI-DRKI2129-VGVGESELLSCHAFT   PIC  X(0050).
             10 PI-DRKI2129-VGVGEWERBFLPROZ   PIC S9(4) COMP.
             10 PI-DRKI2129-VGVGKLASFLAEPROZ   PIC  X(0020).
             10 PI-DRKI2129-VGVGRUNDFUERUVZ   PIC  X(0040).
             10 PI-DRKI2129-VGVHAGELKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVKELLERVORHKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVKLAUSELTARIF   PIC  X(0080).
             10 PI-DRKI2129-VGVKUENDIGUNGKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVLEISARTLB-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2129-VGVLEISTFREIBDAT   PIC  X(010).
             10 PI-DRKI2129-VGVLEISTFREIVDAT   PIC  X(010).
             10 PI-DRKI2129-VGVLEITUNGSWASKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVMAILINGAKTION   PIC  X(0070).
             10 PI-DRKI2129-VGVMINDBEITRAGKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVMODELL   PIC  X(0018).
             10 PI-DRKI2129-VGVMONOPOLKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVNEBENGEBAEUDE   PIC  X(0080).
             10 PI-DRKI2129-VGVNGEBENTSCHGRZ   PIC S9(9) COMP.
             10 PI-DRKI2129-VGVPRMSATZGEFAHR   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI2129-VGVPRMSATZWAGNIS   PIC S9(2)V9(7) COMP-3.
             10 PI-DRKI2129-VGVPRODUKT   PIC  X(0030).
             10 PI-DRKI2129-VGVRANSCHRHAUSNR   PIC  X(0015).
             10 PI-DRKI2129-VGVRANSCHRIFTLKZ   PIC S9(9) COMP.
             10 PI-DRKI2129-VGVRANSCHRIFTORT   PIC  X(0030).
             10 PI-DRKI2129-VGVRANSCHRIFTPLZ   PIC  X(0006).
             10 PI-DRKI2129-VGVRANSCHRSTR   PIC  X(0040).
             10 PI-DRKI2129-VGVRISIKO   PIC  X(0050).
             10 PI-DRKI2129-VGVROHBAUBISDAT   PIC  X(010).
             10 PI-DRKI2129-VGVROHBAUVER-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2129-VGVSCHWBADGEBKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVSELBSTBTEXT   PIC  X(0014).
             10 PI-DRKI2129-VGVSNUTZFLAECHE   PIC S9(4) COMP.
             10 PI-DRKI2129-VGVSONSTIGES   PIC  X(0080).
             10 PI-DRKI2129-VGVSTAFFPRAEMKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVSTURMKZ   PIC  X(0002).
             10 PI-DRKI2129-VGVUMSTELLDAT   PIC  X(010).
             10 PI-DRKI2129-VGVUNTERVERSVERZ   PIC  X(0002).
             10 PI-DRKI2129-VGVVERSNUMMER   PIC  X(0040).
             10 PI-DRKI2129-VGVVERSSUM1914   PIC S9(9) COMP.
             10 PI-DRKI2129-VGVWOHNFLAECHE   PIC S9(9) COMP.
             10 PI-DRKI2129-VGVZELEMENTSCH   PIC S9(4) COMP.
             10 PI-DRKI2129-VGVZERDBEBEN   PIC S9(4) COMP.
             10 PI-DRKI2129-VGVZLEITWASSER   PIC S9(4) COMP.
             10 PI-DRKI2129-VGVZSTURM   PIC S9(4) COMP.
             10 PI-DRKI2129-VGVZUEBERSCHWEM   PIC S9(4) COMP.
             10 PI-DRKI2129-VGVZUSATZINFO   PIC  X(0100).
END   *
 
      *
       01  PO-DRKI2129.
           05  PO-DRKI2129-RC                     PIC  X(003).
               88 PO-DRKI2129-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2129-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

