      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  05.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3515.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3515.CPv  $
      *
      *   Rev 2.3   Jun 08 2020 08:55:42   t13ae31
      *AKI01779: KFZ Erweiterungen zu RT06 OZP´s
      *
      *   Rev 2.2   Jan 22 2019 14:37:26   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.1   Dec 19 2018 09:40:42   t13ex78
      *AKI01641: OZPs & Copies
      *
      *   Rev 2.0   Nov 05 2018 08:54:16   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01641!06.11.18!1.0 ! NEU                       ! P.Hübner *
      * -------------------------------------------------------------*
      * AKI01779!18.05.20!    ! neu FIRMENNR ELEKTROKZ   ! P. Hübner *
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * -----------!--------!------------------------------!----------
      * DBO-10494  !17.10.22! Vergrößerung G11VZUNACH von  ! T13EX78
      *            !        ! 370 auf 925 Byte             !
      * -----------!--------!------------------------------!----------
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI3515                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
 
       01  PI-DRKI3515.
 
           05  PI-DRKI3515-PROGRAMM          PIC  X(008).
           05  PI-DRKI3515-UFKT-CODE         PIC  9(002).
           05  PI-DRKI3515-DATEN.
             10 PI-DRKI3515-G11VVERTRAGSNR   PIC  X(0015).
             10 PI-DRKI3515-G11VMANDANT      PIC  S9(9) COMP.
 
      *
       01  PO-DRKI3515.
           05  PO-DRKI3515-RC                     PIC  X(003).
               88 PO-DRKI3515-RC-OK               VALUE 'OK '.
               88 PO-DRKI3515-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI3515-RC-EOF              VALUE 'EOF'.
      *
           05  PO-DRKI3515-DATEN.
 
             10 PO-DRKI3515-G11VVERTRAGSNR   PIC  X(0015).
             10 PO-DRKI3515-G11VMANDANT      PIC  S9(9) COMP.
             10 PO-DRKI3515-G11BEGLFAHRTXT   PIC  X(10).
             10 PO-DRKI3515-G11FALTERMAX     PIC  X(0002).
             10 PO-DRKI3515-G11KFZLEISTUNG   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3515-G11STORNODAT     PIC  X(0008).
             10 PO-DRKI3515-G11VABLAUF       PIC  X(0008).
             10 PO-DRKI3515-G11VABSCHLVBNR   PIC S9(9) COMP.
             10 PO-DRKI3515-G11VAENDDAT      PIC  X(0008).
             10 PO-DRKI3515-G11VAENDKZ       PIC  X(0002).
             10 PO-DRKI3515-G11VAKZARTKNR    PIC  X(0001).
             10 PO-DRKI3515-G11VALTERGRPKNR  PIC  X(0004).
             10 PO-DRKI3515-G11VAMTKZ        PIC  X(0012).
             10 PO-DRKI3515-G11VBEGINN       PIC  X(0008).
             10 PO-DRKI3515-G11VBEGLFAHR     PIC  X(0002).
             10 PO-DRKI3515-G11VBEITRAG      PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3515-G11VBEITRKFZW    PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3515-G11VBEITRKHZW    PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3515-G11VBESVEREIN.
                49 PO-DRKI3515-G11VBESVEREIN-LEN  PIC S9(4) COMP.
                49 PO-DRKI3515-G11VBESVEREIN-TEXT PIC X(750).
             10 PO-DRKI3515-G11VBRANCHE      PIC  X(0060).
             10 PO-DRKI3515-G11VBUENDKZ      PIC  X(0001).
             10 PO-DRKI3515-G11VCOCDAT       PIC  X(0008).
             10 PO-DRKI3515-G11VCOCKZ        PIC  X(0001).
             10 PO-DRKI3515-G11VDAKF         PIC  X(0002).
             10 PO-DRKI3515-G11VDAKH         PIC  X(0002).
             10 PO-DRKI3515-G11VERSTZULDAT   PIC  X(0008).
             10 PO-DRKI3515-G11VFAHRER       PIC  X(0010).
             10 PO-DRKI3515-G11VFAHRGEBIET   PIC  X(0010).
             10 PO-DRKI3515-G11VFAHRZGID     PIC  X(0020).
             10 PO-DRKI3515-G11VFIRMENNR     PIC  X(0006).
             10 PO-DRKI3515-G11VFSCHEINDAT   PIC  X(0010).
             10 PO-DRKI3515-G11VFZGABSTELL   PIC  X(0010).
             10 PO-DRKI3515-G11VFZGARTKNR    PIC  X(0003).
             10 PO-DRKI3515-G11VFZGAUFBAU    PIC  X(0004).
             10 PO-DRKI3515-G11VFZGHALTER    PIC  X(0002).
             10 PO-DRKI3515-G11VFZGHALTNAME  PIC  X(0040).
             10 PO-DRKI3515-G11VFZGHALTVNAME PIC  X(0040).
             10 PO-DRKI3515-G11VFZGMODELL    PIC  X(0050).
             10 PO-DRKI3515-G11VFZGNUTZ      PIC  X(0010).
             10 PO-DRKI3515-G11VFZGVERWKNR   PIC  X(0003).
             10 PO-DRKI3515-G11VFZHGHALTTXT  PIC  X(0010).
             10 PO-DRKI3515-G11VGEBDATFAH    PIC  X(0010).
             10 PO-DRKI3515-G11VGESGEWICHT   PIC S9(7)V COMP-3.
             10 PO-DRKI3515-G11VHERSTNR      PIC  X(0004).
             10 PO-DRKI3515-G11VHERSTTXT     PIC  X(0025).
             10 PO-DRKI3515-G11VJAHRKMLEIST  PIC S9(7)V COMP-3.
             10 PO-DRKI3515-G11VKFZKW        PIC S9(4)V COMP-3.
             10 PO-DRKI3515-G11VKFZLEISTKZ   PIC  X(0001).
             10 PO-DRKI3515-G11VKFZTYP       PIC  X(0003).
             10 PO-DRKI3515-G11VKMSTAAKT     PIC S9(7)V COMP-3.
             10 PO-DRKI3515-G11VKMSTAAKTDAT  PIC  X(0008).
             10 PO-DRKI3515-G11VKSMVERTR.
                49 PO-DRKI3515-G11VKSMVERTR-LEN  PIC S9(4) COMP.
                49 PO-DRKI3515-G11VKSMVERTR-TEXT PIC X(464).
             10 PO-DRKI3515-G11VLEASING      PIC  X(0001).
             10 PO-DRKI3515-G11VLETZTAEND    PIC  X(0008).
             10 PO-DRKI3515-G11VLSVSCOR      PIC  X(0001).
             10 PO-DRKI3515-G11VNAESPEZKNR   PIC  X(0003).
             10 PO-DRKI3515-G11VNEUWERT      PIC S9(7)V COMP-3.
             10 PO-DRKI3515-G11VNUTZL        PIC S9(7)V COMP-3.
             10 PO-DRKI3515-G11VPAKETTEXT    PIC  X(0150).
             10 PO-DRKI3515-G11VPAKETTYP     PIC  X(0004).
             10 PO-DRKI3515-G11VPIN          PIC  X(0009).
             10 PO-DRKI3515-G11VPINPRUEF     PIC  X(0002).
             10 PO-DRKI3515-G11VPLATZ        PIC  X(0003).
             10 PO-DRKI3515-G11VRKLKF        PIC  X(0004).
             10 PO-DRKI3515-G11VRKLKH        PIC  X(0004).
             10 PO-DRKI3515-G11VSAISONBEG    PIC  X(0002).
             10 PO-DRKI3515-G11VSAISONEND    PIC  X(0002).
             10 PO-DRKI3515-G11VSAISONKZ     PIC  X(0001).
             10 PO-DRKI3515-G11VSBTK         PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3515-G11VSBVK         PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3515-G11VSCHUTZBR     PIC  X(0010).
             10 PO-DRKI3515-G11VSCOREDAT     PIC  X(0008).
             10 PO-DRKI3515-G11VSCOREWERT    PIC S9(3)V COMP-3.
             10 PO-DRKI3515-G11VSFRTAB.
                49 PO-DRKI3515-G11VSFRTAB-LEN  PIC S9(4) COMP.
                49 PO-DRKI3515-G11VSFRTAB-TEXT PIC X(30).
             10 PO-DRKI3515-G11VSONDERAUS.
                49 PO-DRKI3515-G11VSONDERAUS-LEN  PIC S9(4) COMP.
                49 PO-DRKI3515-G11VSONDERAUS-TEXT PIC X(240).
             10 PO-DRKI3515-G11VTARAUSW      PIC  X(0001).
             10 PO-DRKI3515-G11VTARIFBEZ.
                49 PO-DRKI3515-G11VTARIFBEZ-LEN  PIC S9(4) COMP.
                49 PO-DRKI3515-G11VTARIFBEZ-TEXT PIC X(21).
             10 PO-DRKI3515-G11VTELEMATIK    PIC S9(14)V COMP-3.
             10 PO-DRKI3515-G11VTYPKLKF      PIC  X(0002).
             10 PO-DRKI3515-G11VTYPKLKH      PIC  X(0002).
             10 PO-DRKI3515-G11VVERSBED.
                49 PO-DRKI3515-G11VVERSBED-LEN  PIC S9(4) COMP.
                49 PO-DRKI3515-G11VVERSBED-TEXT PIC X(63).
             10 PO-DRKI3515-G11VWAEHRKZ      PIC  X(0003).
             10 PO-DRKI3515-G11VWAGNISKZ     PIC  X(0003).
             10 PO-DRKI3515-G11VWAGNISTXT    PIC  X(0025).
             10 PO-DRKI3515-G11VWERKBIND     PIC  X(0001).
             10 PO-DRKI3515-G11VWOHNEIGEN    PIC  X(0010).
             10 PO-DRKI3515-G11VZAHLART      PIC  X(0001).
             10 PO-DRKI3515-G11VZAHLWEISE    PIC  X(0001).
             10 PO-DRKI3515-G11VZULDATUM     PIC  X(0008).
             10 PO-DRKI3515-G11VELEKTROKZ    PIC  X(0001).
             10 PO-DRKI3515-G11VZUNACH.
                49 PO-DRKI3515-G11VZUNACH-LEN  PIC S9(4) COMP.
                49 PO-DRKI3515-G11VZUNACH-TEXT PIC X(925).
             10 PO-DRKI3515-G11VZUSTAND      PIC  X(0001).
             10 PO-DRKI3515-G11VZWSCHL       PIC  X(0002).
             10 PO-DRKI3515-G11VZWSCHLTXT    PIC  X(0003).
END   *
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 


