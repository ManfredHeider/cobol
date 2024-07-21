      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0833.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0833.CPv  $
      *
      *   Rev 2.4   Sep 10 2012 13:08:04   t13ae30
      *ASL00462: Versorgungswerk 2012 - Anpassung F&A-SST
      *
      *   Rev 2.3   Oct 14 2009 13:29:42   t13ae30
      *AAS00862: Erweiterung Access-Datenbank 
      *
      *   Rev 2.2   Nov 05 2008 16:31:24   t13ae30
      *ASL00309: Redesign Versorgungswerk
      *
      *   Rev 2.1   Oct 18 2006 11:41:30   t13ae30
      *AVB00882: F&A-SST, Vers.Nr. für VBZ
      *
      *   Rev 2.0   Sep 05 2006 13:07:56   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0833                    *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    05.09.2006                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00882!05.09.06! 2.1! NEU                       !TH.WILLIG *
      * ASL00309!04.11.08! 2.2! ERW. FUNKTION DRVB0833    !TH.WILLIG *
      * ASS00862!13.10.09! 2.3! ERW. FUNKTION DRVB0833    !TH.WILLIG *
      * ASL00462!05.09.12! 2.4! ERW. FUNKTION DRVB0833    !TH.WILLIG *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0833: LESEN MA-ST-NR
      *                               ERM. VSW-VERS-NR
      ****************************************************************
      *
       01  PI-DRVB0833.
           05  PI-DRVB0833-DATEN.
               10 PI-DRVB0833-ST-NR            PIC S9(009) COMP.
      *
       01  PO-DRVB0833.
           05  PO-DRVB0833-RC                  PIC  X(003).
               88 PO-DRVB0833-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0833-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0833-NI.
               10 PO-DRVB0833-NI-VSWZ-G-VERSNR       PIC S9(004) COMP.
               10 PO-DRVB0833-NI-VSWZ-Z-VERSNR       PIC S9(004) COMP.
               10 PO-DRVB0833-NI-VSWZ-G-RVVERSNR     PIC S9(004) COMP.
               10 PO-DRVB0833-NI-VSWZ-Z-RVVERSNR     PIC S9(004) COMP.
               10 PO-DRVB0833-NI-VSWZ-DBI-KTO-NR     PIC S9(004) COMP.
041108         10 PO-DRVB0833-NI-VSWZ-AGI-KTO-NR     PIC S9(004) COMP.
131009         10 PO-DRVB0833-NI-VSWZ-AGI-KT-NR2     PIC S9(004) COMP.
050912         10 PO-DRVB0833-NI-VSWZ-GVA-VERSNR     PIC S9(004) COMP.
050912         10 PO-DRVB0833-NI-VSWZ-G-VA-VERSB     PIC S9(004) COMP.
050912         10 PO-DRVB0833-NI-VSWZ-G-VA-VSUM      PIC S9(004) COMP.
050912         10 PO-DRVB0833-NI-VSWZ-ZVA-VERSNR     PIC S9(004) COMP.
050912         10 PO-DRVB0833-NI-VSWZ-Z-VA-VERSB     PIC S9(004) COMP.
050912         10 PO-DRVB0833-NI-VSWZ-Z-VA-VSUM      PIC S9(004) COMP.
050912         10 PO-DRVB0833-NI-VSWZ-AVA-VERSNR     PIC S9(004) COMP.
050912         10 PO-DRVB0833-NI-VSWZ-A-VA-VERSB     PIC S9(004) COMP.
050912         10 PO-DRVB0833-NI-VSWZ-A-VA-VSUM      PIC S9(004) COMP.
           05  PO-DRVB0833-DATEN.
               10 PO-DRVB0833-VSWZ-G-VERSNR          PIC  9(015).
               10 PO-DRVB0833-VSWZ-Z-VERSNR          PIC  9(015).
               10 PO-DRVB0833-VSWZ-G-RVVERSNR        PIC  9(015).
               10 PO-DRVB0833-VSWZ-Z-RVVERSNR        PIC  9(015).
               10 PO-DRVB0833-VSWZ-DBI-KTO-NR        PIC  X(009).
041108         10 PO-DRVB0833-VSWZ-AGI-KTO-NR        PIC  X(010).
131009         10 PO-DRVB0833-VSWZ-AGI-KT-NR2        PIC  X(010).
050912         10 PO-DRVB0833-VSWZ-G-VA-VERS-NR      PIC  9(015).
050912         10 PO-DRVB0833-VSWZ-G-VA-VERSB        PIC  9(009)V99.
050912         10 PO-DRVB0833-VSWZ-G-VA-VSUM         PIC  9(009)V99.
050912         10 PO-DRVB0833-VSWZ-Z-VA-VERS-NR      PIC  9(015).
050912         10 PO-DRVB0833-VSWZ-Z-VA-VERSB        PIC  9(009)V99.
050912         10 PO-DRVB0833-VSWZ-Z-VA-VSUM         PIC  9(009)V99.
050912         10 PO-DRVB0833-VSWZ-A-VA-VERS-NR      PIC  9(015).
050912         10 PO-DRVB0833-VSWZ-A-VA-VERSB        PIC  9(009)V99.
050912         10 PO-DRVB0833-VSWZ-A-VA-VSUM         PIC  9(009)V99.
      *
      ******* ENDE COPY-MEMBER **************************************
