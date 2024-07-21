000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WUPA0017.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/WUPA0017.CPv  $
000140*
000150*   Rev 2.1   Sep 11 2001 09:49:40   T13EX31
000160*APA00143 - Euro-Umstellung: Datenmigration
000170*
000180*   Rev 2.0   10 Sep 2001 15:27:44   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220******************************************************************
000230* COBOL DECLARATION FOR TABLE                                    *
000240* TBPA0017                                                       *
000250******************************************************************
000260     05  DCL-TBPA0017.
000270         10 PDTA-ID           PIC S9(15)V USAGE COMP-3.
000280         10 PGEB-GES          PIC S9(4) USAGE COMP.
000290         10 KNR-DTA-ART       PIC S9(4) USAGE COMP.
000300         10 KNR-DTA-SPLIT-ART PIC S9(4) USAGE COMP.
000310         10 PDTA-ORIGINAL-ID  PIC S9(15)V USAGE COMP-3.
000320         10 PDTA-ERSTELL-DAT  PIC X(10).
000330         10 PDTA-TRANSF-DAT   PIC X(10).
000340         10 PDTA-STAND-DAT    PIC X(10).
000350         10 PDTA-ANZ-SATZ     PIC S9(9) USAGE COMP.
000360         10 PDTA-ANZ-FEHL     PIC S9(9) USAGE COMP.
000370         10 PDTA-KONTR-WERT   PIC S9(11)V9(2) USAGE COMP-3.
000380         10 PDTA-INHALT       PIC X(40).
000390         10 PDTA-VSTAT        PIC X(4).
000400         10 PDTA-LAST-UPDATE  PIC X(26).
000410         10 PDTA-KENNUNG      PIC X(8).
000420******************************************************************
000430* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
000440******************************************************************
