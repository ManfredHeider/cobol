      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01567!26.02.18!1.0 ! NEU                       ! P.Hübner *
      * -------------------------------------------------------------*
      * AKI01779!15.05.20!    ! neu FIRMENNR ELEKTROKZ    ! P. Hübner*
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-10064  !31.05.22! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    G10VGEBDAT-VN             !          
      * -----------!--------!------------------------------!----------
      * DBO-10494  !17.10.22! Vergrößerung G10VZUNACH von  ! T13EX78
      *            !        ! 370 auf 925 Byte             !
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI3297                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
 
       01  PI-DRKI3297.
 
           05  PI-DRKI3297-PROGRAMM          PIC  X(008).
           05  PI-DRKI3297-UFKT-CODE         PIC  9(002).
           05  PI-DRKI3297-DATEN.
             10 PI-DRKI3297-G10VVERTRAGSNR   PIC  X(0015).
             10 PI-DRKI3297-G10VMANDANT      PIC  S9(9) COMP.
 
      *
       01  PO-DRKI3297.
           05  PO-DRKI3297-RC                     PIC  X(003).
               88 PO-DRKI3297-RC-OK               VALUE 'OK '.
               88 PO-DRKI3297-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI3297-RC-EOF              VALUE 'EOF'.
      *
           05  PO-DRKI3297-DATEN.
 
             10 PO-DRKI3297-G10VVERTRAGSNR   PIC  X(0015).
             10 PO-DRKI3297-G10VMANDANT      PIC  S9(9) COMP.
             10 PO-DRKI3297-G10BEGLFAHRTXT   PIC  X(10).
             10 PO-DRKI3297-G10FALTERMAX     PIC  X(0002).
             10 PO-DRKI3297-G10KFZLEISTUNG   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3297-G10STORNODAT     PIC  X(0008).
             10 PO-DRKI3297-G10VABLAUF       PIC  X(0008).
             10 PO-DRKI3297-G10VABSCHLVBNR   PIC S9(9) COMP.
             10 PO-DRKI3297-G10VAENDDAT      PIC  X(0008).
             10 PO-DRKI3297-G10VAENDKZ       PIC  X(0002).
             10 PO-DRKI3297-G10VAKZARTKNR    PIC  X(0001).
             10 PO-DRKI3297-G10VALTERGRPKNR  PIC  X(0004).
             10 PO-DRKI3297-G10VAMTKZ        PIC  X(0012).
             10 PO-DRKI3297-G10VBEGINN       PIC  X(0008).
             10 PO-DRKI3297-G10VBEGLFAHR     PIC  X(0002).
             10 PO-DRKI3297-G10VBEITRAG      PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3297-G10VBEITRKFZW    PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3297-G10VBEITRKHZW    PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3297-G10VBESVEREIN.
                49 PO-DRKI3297-G10VBESVEREIN-LEN  PIC S9(4) COMP.
                49 PO-DRKI3297-G10VBESVEREIN-TEXT PIC X(750).
             10 PO-DRKI3297-G10VBRANCHE      PIC  X(0060).
             10 PO-DRKI3297-G10VBUENDKZ      PIC  X(0001).
             10 PO-DRKI3297-G10VCOCDAT       PIC  X(0008).
             10 PO-DRKI3297-G10VCOCKZ        PIC  X(0001).
             10 PO-DRKI3297-G10VDAKF         PIC  X(0002).
             10 PO-DRKI3297-G10VDAKH         PIC  X(0002).
             10 PO-DRKI3297-G10VELEKTROKZ    PIC  X(0001).
             10 PO-DRKI3297-G10VERSTZULDAT   PIC  X(0008).
             10 PO-DRKI3297-G10VFAHRER       PIC  X(0010).
             10 PO-DRKI3297-G10VFAHRGEBIET   PIC  X(0010).
             10 PO-DRKI3297-G10VFAHRZGID     PIC  X(0020).
             10 PO-DRKI3297-G10VFIRMENNR     PIC  X(0006).
             10 PO-DRKI3297-G10VFSCHEINDAT   PIC  X(0010).
             10 PO-DRKI3297-G10VFZGABSTELL   PIC  X(0010).
             10 PO-DRKI3297-G10VFZGARTKNR    PIC  X(0003).
             10 PO-DRKI3297-G10VFZGAUFBAU    PIC  X(0004).
             10 PO-DRKI3297-G10VFZGHALTER    PIC  X(0002).
             10 PO-DRKI3297-G10VFZGHALTNAME  PIC  X(0040).
             10 PO-DRKI3297-G10VFZGHALTVNAME PIC  X(0040).
             10 PO-DRKI3297-G10VFZGMODELL    PIC  X(0050).
             10 PO-DRKI3297-G10VFZGNUTZ      PIC  X(0010).
             10 PO-DRKI3297-G10VFZGVERWKNR   PIC  X(0003).
             10 PO-DRKI3297-G10VFZHGHALTTXT  PIC  X(0010).
             10 PO-DRKI3297-G10VGEBDAT-VN    PIC  X(0010).
             10 PO-DRKI3297-G10VGEBDATFAH    PIC  X(0010).
             10 PO-DRKI3297-G10VGESGEWICHT   PIC S9(7)V COMP-3.
             10 PO-DRKI3297-G10VHERSTNR      PIC  X(0004).
             10 PO-DRKI3297-G10VHERSTTXT     PIC  X(0025).
             10 PO-DRKI3297-G10VJAHRKMLEIST  PIC S9(7)V COMP-3.
             10 PO-DRKI3297-G10VKFZKW        PIC S9(4)V COMP-3.
             10 PO-DRKI3297-G10VKFZLEISTKZ   PIC  X(0001).
             10 PO-DRKI3297-G10VKFZTYP       PIC  X(0003).
             10 PO-DRKI3297-G10VKMSTAAKT     PIC S9(7)V COMP-3.
             10 PO-DRKI3297-G10VKMSTAAKTDAT  PIC  X(0008).
             10 PO-DRKI3297-G10VKSMVERTR.
                49 PO-DRKI3297-G10VKSMVERTR-LEN  PIC S9(4) COMP.
                49 PO-DRKI3297-G10VKSMVERTR-TEXT PIC X(464).
             10 PO-DRKI3297-G10VLEASING      PIC  X(0001).
             10 PO-DRKI3297-G10VLETZTAEND    PIC  X(0008).
             10 PO-DRKI3297-G10VLSVSCOR      PIC  X(0001).
             10 PO-DRKI3297-G10VNAESPEZKNR   PIC  X(0003).
             10 PO-DRKI3297-G10VNEUWERT      PIC S9(7)V COMP-3.
             10 PO-DRKI3297-G10VNUTZL        PIC S9(7)V COMP-3.
             10 PO-DRKI3297-G10VPAKETTEXT    PIC  X(0150).
             10 PO-DRKI3297-G10VPAKETTYP     PIC  X(0004).
             10 PO-DRKI3297-G10VPIN          PIC  X(0009).
             10 PO-DRKI3297-G10VPINPRUEF     PIC  X(0002).
             10 PO-DRKI3297-G10VPLATZ        PIC  X(0003).
             10 PO-DRKI3297-G10VRKLKF        PIC  X(0004).
             10 PO-DRKI3297-G10VRKLKH        PIC  X(0004).
             10 PO-DRKI3297-G10VSAISONBEG    PIC  X(0002).
             10 PO-DRKI3297-G10VSAISONEND    PIC  X(0002).
             10 PO-DRKI3297-G10VSAISONKZ     PIC  X(0001).
             10 PO-DRKI3297-G10VSBTK         PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3297-G10VSBVK         PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3297-G10VSCHUTZBR     PIC  X(0010).
             10 PO-DRKI3297-G10VSCOREDAT     PIC  X(0008).
             10 PO-DRKI3297-G10VSCOREWERT    PIC S9(3)V COMP-3.
             10 PO-DRKI3297-G10VSFRTAB.
                49 PO-DRKI3297-G10VSFRTAB-LEN  PIC S9(4) COMP.
                49 PO-DRKI3297-G10VSFRTAB-TEXT PIC X(30).
             10 PO-DRKI3297-G10VSONDERAUS.
                49 PO-DRKI3297-G10VSONDERAUS-LEN  PIC S9(4) COMP.
                49 PO-DRKI3297-G10VSONDERAUS-TEXT PIC X(240).
             10 PO-DRKI3297-G10VTARAUSW      PIC  X(0001).
             10 PO-DRKI3297-G10VTARIFBEZ.
                49 PO-DRKI3297-G10VTARIFBEZ-LEN  PIC S9(4) COMP.
                49 PO-DRKI3297-G10VTARIFBEZ-TEXT PIC X(21).
             10 PO-DRKI3297-G10VTELEMATIK    PIC S9(14)V COMP-3.
             10 PO-DRKI3297-G10VTYPKLKF      PIC  X(0002).
             10 PO-DRKI3297-G10VTYPKLKH      PIC  X(0002).
             10 PO-DRKI3297-G10VVERSBED.
                49 PO-DRKI3297-G10VVERSBED-LEN  PIC S9(4) COMP.
                49 PO-DRKI3297-G10VVERSBED-TEXT PIC X(63).
             10 PO-DRKI3297-G10VWAEHRKZ      PIC  X(0003).
             10 PO-DRKI3297-G10VWAGNISKZ     PIC  X(0003).
             10 PO-DRKI3297-G10VWAGNISTXT    PIC  X(0025).
             10 PO-DRKI3297-G10VWERKBIND     PIC  X(0001).
             10 PO-DRKI3297-G10VWOHNEIGEN    PIC  X(0010).
             10 PO-DRKI3297-G10VZAHLART      PIC  X(0001).
             10 PO-DRKI3297-G10VZAHLWEISE    PIC  X(0001).
             10 PO-DRKI3297-G10VZULDATUM     PIC  X(0008).
             10 PO-DRKI3297-G10VZUNACH.
                49 PO-DRKI3297-G10VZUNACH-LEN  PIC S9(4) COMP.
                49 PO-DRKI3297-G10VZUNACH-TEXT PIC X(925).
             10 PO-DRKI3297-G10VZUSTAND      PIC  X(0001).
             10 PO-DRKI3297-G10VZWSCHL       PIC  X(0002).
             10 PO-DRKI3297-G10VZWSCHLTXT    PIC  X(0003).
END   *
 
      *
      ******* ENDE COPY-MEMBER ***************************************