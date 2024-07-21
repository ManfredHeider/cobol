000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   DRVB0025.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/DRVB0025.CPv  $
000140*
000150*   Rev 2.1   21 Sep 2001 10:43:06   t13ae13
000160*AVB00534: Zweite PIN-SST für VB-Portal
000170*
000180*   Rev 2.0   18 Sep 2001 12:17:30   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220*    COPY-MEMBER: DRVB0025                                      *
000230*    --------------------------------------------------------   *
000240*    SCHNITTSTELLE FÜR DAS UPRO ZXVB0029                        *
000250*    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
000260*    --------------------------------------------------------   *
000270*    ERSTELLT: SCHMIDT                                          *
000280*    DATUM:    15.04.1999                                       *
000290*****************************************************************
000300*    VERSION  1
000310****************************************************************
000320* AENDERUNGEN:                                                 *
000330* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
000340* -------------------------------------------------------------*
000350* AAS00060!15.04.99! 01 ! NEUERSTELLUNG             ! SCHMIDT,B*
000360*         !        !    !                           !          *
000370*         !        !    !                           !          *
000380*                                                              *
000390* ENDE-LOGBUCH                                                 *
000400****************************************************************
000410****************************************************************
000420*    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
000430*    FÜR DIE FUNKTION DRVB0025                                 *
000440*    AUSSER: DEM FUNKTIONSNAMEN,                               *
000450*            DEM ERR-ERROR-BEREICH UND                         *
000460*            DEM STANDARD-STATUS-BEREICH                       *
000470*            (JEWEILS SEPARATE COPYS)                          *
000480*                                                              *
000490****************************************************************
000500*
000510 01 PI-DRVB0025.
000520    05 PI-DRVB0025-PROGRAMM           PIC  X(008).
000530    05 PI-DRVB0025-UFKT-CODE          PIC  9(002).
000540    05 PI-DRVB0025-MA-ST-NR           PIC S9(9)    COMP.
000550    05 PI-DRVB0025-KTY-SEMZ-TYP       PIC S9(4)    COMP.
000560*
000570 01 PO-DRVB0025.

000590    05 PO-DRVB0025-RC                 PIC  X(003).
000600       88 PO-DRVB0025-RC-OK                        VALUE 'OK '.
000610       88 PO-DRVB0025-RC-LEER                      VALUE 'LER'.
000620       88 PO-DRVB0025-RC-EOF                       VALUE 'EOF'.
000630       88 PO-DRVB0025-RC-ERR                       VALUE 'ERR'.

000650    05 PO-DRVB0025-DATEN.
000660       10 PO-DRVB0025-KNR-SEMZ-TYP       PIC S9(4)    COMP.
000670       10 PO-DRVB0025-SEMZ-DAT-VON       PIC X(010).
000680*
000690***** ENDE-COPY-MEMBER *****************************************

