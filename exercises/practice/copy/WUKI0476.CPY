      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11/02/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0476.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0476.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 15:11:56   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.1   Jul 09 2015 10:47:54   t13ex73
      *AKI01342: DVM G90 Erstellung - Verarbeitung Copybooks
      *
      *   Rev 2.0   Feb 11 2015 14:19:04   T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-G90KSDSVERTRAG.
           10 G90VVERTRAGSNR                PIC  X(0015).
           10 G90VMANDANT                   PIC S9(9) COMP.
           10 G90VAENDKZ                    PIC  X(0002).
           10 G90VGESID                     PIC S9(4) COMP.
           10 G90VABSCHLVBNR                PIC S9(9) COMP.
           10 G90VPRODUKTART                PIC S9(4) COMP.
           10 G90VABRGEBSATZ                PIC S9(1)V9(2) COMP-3.
           10 G90VFACTGEBSATZ               PIC S9(1)V9(2) COMP-3.
           10 G90VFACTANKBETR               PIC S9(10)V9(2) COMP-3.
           10 G90VBEGINN                    PIC  X(010).
           10 G90VENDE                      PIC  X(010).
           10 G90VEINRGEBBETR               PIC S9(8)V9(2) COMP-3.
           10 G90VIBAN                      PIC  X(0034).
           10 G90VMANDATREFNR               PIC  X(0035).
           10 G90VABWBZKZ                   PIC  X(0001).
           10 G90VABWBZVNAME                PIC  X(0030).
           10 G90VABWBZNNAME                PIC  X(0030).
           10 G90VABWBZSTRASSE              PIC  X(0040).
           10 G90VABWBZPLZ                  PIC S9(9) COMP.
           10 G90VABWBZORT                  PIC  X(0030).
           10 G90VINFOSPERRE                PIC  X(0001).
           10 G90VPROGVONDAT                PIC  X(010).
           10 G90VPROGBISDAT                PIC  X(010).
           10 G90VPROGANZRECHN              PIC S9(9) COMP.
           10 G90VPROGGESUMS                PIC S9(8)V9(2) COMP-3.
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).

