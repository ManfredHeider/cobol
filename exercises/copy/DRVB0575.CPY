000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   DRVB0575.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/DRVB0575.CPv  $
000140*
000150*   Rev 2.1   05 Mar 2002 11:18:20   t13ae13
000160*ASL00135: Dir. herausg. D werden zeitlich unbegrenzt gewertet
000170*
000180*   Rev 2.0   Feb 25 2002 17:31:36   p13sp20
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220*---------------------------------------------------------------*
000230*    COPY-MEMBER: DRVB0575                                      *
000240*    --------------------------------------------------------   *
000250*    SCHNITTSTELLE FÜR DAS UPRO ZJVB0015                        *
000260*    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
000270*    --------------------------------------------------------   *
000280*    ERSTELLT: S. Walb                                          *
000290*    DATUM:    27.02.2002                                       *
000300*****************************************************************
000310*    VERSION  1
000320****************************************************************
000330* AENDERUNGEN:                                                 *
000340* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
000350* -------------------------------------------------------------*
000360* ASL00135!27.02.02! 01 ! NEUERSTELLUNG             ! S. Walb  *
000370*         !        !    !                           !          *
000380*         !        !    !                           !          *
000390*                                                              *
000400* ENDE-LOGBUCH                                                 *
000410****************************************************************
000420****************************************************************
000430*    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
000440*    FÜR DIE FUNKTION DRVB0575                                 *
000450*    AUSSER: DEM FUNKTIONSNAMEN,                               *
000460*            DEM ERR-ERROR-BEREICH UND                         *
000470*            DEM STANDARD-STATUS-BEREICH                       *
000480*            (JEWEILS SEPARATE COPYS)                          *
000490*                                                              *
000500****************************************************************
000510*
000520*
000530 01 PI-DRVB0575.
000540    05 PI-DRVB0575-PROGRAMM           PIC  X(008).
000550    05 PI-DRVB0575-UFKT-CODE          PIC  9(002).
000560    05 PI-DRVB0575-ED-BESITZER        PIC  S9(009) COMP.
000570    05 PI-DRVB0575-ZEITPUNKT          PIC  X(010).
000580*
000590 01 PO-DRVB0575.

000610    05 PO-DRVB0575-RC                 PIC  X(003).
000620       88 PO-DRVB0575-RC-OK                        VALUE 'OK '.
000630       88 PO-DRVB0575-RC-LEER                      VALUE 'LER'.
000640       88 PO-DRVB0575-RC-EOF                       VALUE 'EOF'.
000650       88 PO-DRVB0575-RC-NOT-FOUND                 VALUE 'NFD'.

000670    05 PO-DRVB0575-DATEN.
000680       10 PO-DRVB0575-ED-BETREUTER     PIC  S9(009) COMP.
000690       10 PO-DRVB0575-MS-NR            PIC  S9(004) COMP.
000700*
000710***** ENDE COPY-MEMBER DRVB0575 *****************************
