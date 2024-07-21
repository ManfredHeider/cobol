      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3321.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3321.CPv  $
      *
      *   Rev 2.2   Apr 15 2020 08:17:12   t13ex78
      *AKI01771: List-OZP ZXKI0492
      *
      *   Rev 2.1   May 24 2018 08:02:44   t13ae31
      *AKI01575: G70 Umstellung KSDS auf DB2 OZP Vertrag
      *
      *   Rev 2.0   Mar 09 2018 10:21:10   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01575!12.03.18!1.0 !NEU                        ! P.Hübner *
      * -------------------------------------------------------------*
      * AKI01771! Umbenennung diverser Felder             ! T13EX78  *
      *         ! Teils neue Schlüssel (SCHL-WOHN-PROG)   !          *
      *         ! 5 komplett neue Felder                  !          *
      *----------------------------------------------------------------*
      * DBO-9564!21.12.21!     ! NEUES FELD               ! T13EX39    *
      *         !        !     ! PE-AUSZAHLUNGSART        !            *   
      *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  PI-DRKI3321.
           05  PI-DRKI3321-PROGRAMM     PIC X(008).
           05  PI-DRKI3321-UFKT-CODE    PIC 9(002).
           05  PI-DRKI3321-DATEN.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3321-G70VVERTRAGSNR   PIC  X(0015).
             10 PI-DRKI3321-G70VLFDNR        PIC  X(0001).
             10 PI-DRKI3321-G70VMANDANT PIC  S9(009) COMP.
END   *
 
      *
       01  PO-DRKI3321.
           05  PO-DRKI3321-RC                     PIC  X(003).
               88 PO-DRKI3321-RC-OK               VALUE 'OK '.
               88 PO-DRKI3321-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI3321-RC-EOF              VALUE 'EOF'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3321-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3321-G70VVERTRAGSNR   PIC  X(0015).
             10 PO-DRKI3321-G70VLFDNR        PIC  X(0001).
             10 PO-DRKI3321-G70VMANDANT      PIC S9(9) COMP.
             10 PO-DRKI3321-G70VERMITTKZ     PIC  X(0001).
             10 PO-DRKI3321-G70VMTLEINZBETR  PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3321-G70VDARLHOEHE    PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3321-G70VABBVERS      PIC  X(0007).
             10 PO-DRKI3321-G70VABLAUF       PIC  X(0008).
             10 PO-DRKI3321-G70VABSCHLVBNR   PIC S9(9) COMP.
             10 PO-DRKI3321-G70VABTRETBLZ    PIC  X(0008).
             10 PO-DRKI3321-G70VAENDKZ       PIC  X(0002).
             10 PO-DRKI3321-G70VANFTILG      PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI3321-G70VANNURATE     PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3321-G70VANSPWOP      PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3321-G70VANWZUTEIVO   PIC  X(0008).
             10 PO-DRKI3321-G70VBASERHOEH    PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI3321-G70VBEGINN       PIC  X(0008).
             10 PO-DRKI3321-G70VBEITRAG      PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3321-G70VBEWERTZAHL   PIC S9(3)V9(5) COMP-3.
             10 PO-DRKI3321-G70VBLANKODARL   PIC  X(0001).
             10 PO-DRKI3321-G70VBWZVJ        PIC S9(3)V9(5) COMP-3.
             10 PO-DRKI3321-G70VDARLAKTEKZ   PIC  X(0001).
             10 PO-DRKI3321-G70VDARLSALDO    PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3321-G70VDARLZIEFF    PIC S9(3)V9(3) COMP-3.
             10 PO-DRKI3321-G70VDARLZINOM    PIC S9(3)V9(3) COMP-3.
             10 PO-DRKI3321-G70VDARLZINS     PIC S9(2)V9(3) COMP-3.
             10 PO-DRKI3321-G70VDATAFBEG     PIC  X(0008).
             10 PO-DRKI3321-G70VDATAGEIN     PIC  X(0008).
             10 PO-DRKI3321-G70VDATMINANSP   PIC  X(0008).
             10 PO-DRKI3321-G70VDATSPERFR    PIC  X(0008).
             10 PO-DRKI3321-G70VDATWOBPRAE   PIC  X(0008).
             10 PO-DRKI3321-G70VDATZUTAUSSI  PIC  X(0008).
             10 PO-DRKI3321-G70VDAUZULKZ     PIC  X(0001).
             10 PO-DRKI3321-G70VDISPROZ      PIC S9(2)V9(3) COMP-3.
             10 PO-DRKI3321-G70VDLETZTAKT    PIC  X(0008).
             10 PO-DRKI3321-G70VDYNJAEREINZ  PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI3321-G70VDZINSFAB     PIC  X(0008).
             10 PO-DRKI3321-G70VDZINSFBIS    PIC  X(0008).
             10 PO-DRKI3321-G70VEHELKZ       PIC  X(0001).
             10 PO-DRKI3321-G70VENDBINDFRI   PIC  X(0010).
             10 PO-DRKI3321-G70VENDSTEULZ    PIC  X(0008).
             10 PO-DRKI3321-G70VFINANZKZ     PIC  X(0001).
             10 PO-DRKI3321-G70VFINPROD      PIC  X(0002).
             10 PO-DRKI3321-G70VFMINANSPAR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3321-G70VFOERDRELEV   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3321-G70VFRONTENDT    PIC  X(0008).
             10 PO-DRKI3321-G70VGUTHABBEGR   PIC  X(0008).
             10 PO-DRKI3321-G70VIMMOFITKZ    PIC  X(0001).
             10 PO-DRKI3321-G70VINKASSO      PIC  X(0001).
             10 PO-DRKI3321-G70VJAERLEINZBETR  PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3321-G70VJAERLEINZDAT PIC  X(0008).
             10 PO-DRKI3321-G70VJAHRZULAGE.
                49 PO-DRKI3321-G70VJAHRZULAGE-LEN PIC S9(4) COMP.
                49 PO-DRKI3321-G70VJHRZLG-TEXT PIC X(125).
             10 PO-DRKI3321-G70VKTOSTAKZ     PIC  X(0001).
             10 PO-DRKI3321-G70VLAUFZEIT     PIC  X(0002).
             10 PO-DRKI3321-G70VLETZTAEND    PIC  X(0008).
             10 PO-DRKI3321-G70VLETZTKTOAZ   PIC  X(0008).
             10 PO-DRKI3321-G70VLETZTZUTEIL  PIC  X(0008).
             10 PO-DRKI3321-G70VLZFIN        PIC  X(0003).
             10 PO-DRKI3321-G70VMAXVZINSGH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3321-G70VMEHRZUTEIL   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3321-G70VMIETKAUT     PIC  X(0001).
             10 PO-DRKI3321-G70VMINANSPPRO   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI3321-G70VMINBEWZAHL   PIC S9(4)V9(2) COMP-3.
             10 PO-DRKI3321-G70VMTLEINZDAT   PIC  X(0008).
             10 PO-DRKI3321-G70VNRAKTKTOAZ   PIC  X(0007).
             10 PO-DRKI3321-G70VNVERANLBE    PIC  X(0008).
             10 PO-DRKI3321-G70VONLART       PIC  X(0050).
             10 PO-DRKI3321-G70VONLPROD      PIC  X(0050).
             10 PO-DRKI3321-G70VPARTINFO     PIC  X(0020).
             10 PO-DRKI3321-G70VPASSWORT     PIC  X(0001).
             10 PO-DRKI3321-G70VPRODSCHL     PIC  X(0002).
             10 PO-DRKI3321-G70VSALDOWFKTO   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3321-G70VSCHLWOHNPRG  PIC  X(0002).
             10 PO-DRKI3321-G70VSONDTILJ     PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3321-G70VSPARBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3321-G70VSPARZULAB    PIC  X(0008).
             10 PO-DRKI3321-G70VSTEULFZDAT   PIC  X(0008).
             10 PO-DRKI3321-G70VSUMAVWL      PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3321-G70VSUMFOERVJ    PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3321-G70VSUMME        PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3321-G70VSUMVLEINZ    PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3321-G70VTARIFKZ      PIC  X(0001).
             10 PO-DRKI3321-G70VTARIFUMST    PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3321-G70VVARIWECHS    PIC  X(0001).
             10 PO-DRKI3321-G70VVLPERSONAL   PIC  X(0001).
             10 PO-DRKI3321-G70VVWLKZ        PIC  X(0001).
             10 PO-DRKI3321-G70VWERBVERBOT   PIC  X(0001).
             10 PO-DRKI3321-G70VWKZ          PIC  X(0003).
             10 PO-DRKI3321-G70VWOPKZ        PIC  X(0001).
             10 PO-DRKI3321-G70VWOPVJ        PIC  X(0004).
             10 PO-DRKI3321-G70VWOPVVJ       PIC  X(0004).
             10 PO-DRKI3321-G70VZAHLW        PIC  X(0002).
             10 PO-DRKI3321-G70VZIELTILG     PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI3321-G70VZINSALLG     PIC S9(2)V9(3) COMP-3.
             10 PO-DRKI3321-G70VZINSLFDJ     PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3321-G70VBONUSVORJ    PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3321-G70VZINSVOR      PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3321-G70VBONUS        PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI3321-G70VBONUSFAKT    PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI3321-G70VZPGEN        PIC  X(0002).
             10 PO-DRKI3321-G70VZUTEILANW    PIC  X(0008).
             10 PO-DRKI3321-G70VBONUSART     PIC  X(0002).
             10 PO-DRKI3321-G70VBONUSBEGDATUM PIC  X(0008).
             10 PO-DRKI3321-G70VTUBAKTION    PIC  X(0001).
             10 PO-DRKI3321-G70VSONDERZAHLUNG PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI3321-G70VSONDERTILGUNG PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI3321-G70VPEAUSZAHLART  PIC  X(0001).           


END   *
      *
      ******* ENDE COPY-MEMBER ***************************************


