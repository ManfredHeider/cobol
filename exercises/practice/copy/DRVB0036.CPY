000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   DRVB0036.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/DRVB0036.CPv  $
000140*
000150*   Rev 2.1   29 Nov 2001 10:00:06   t13ae13
000160*ASL00103: EQF-Prüfung entfällt 12 Quartale für D's mit ND's
000170*
000180*   Rev 2.0   25 Sep 2001 12:17:14   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220*---------------------------------------------------------------*
000230*    COPY-MEMBER: DRVB0036                                      *
000240*    --------------------------------------------------------   *
000250*    SCHNITTSTELLE FÜR DAS UPRO ZJVB0015                        *
000260*    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
000270*    --------------------------------------------------------   *
000280*    ERSTELLT: S. Walb                                          *
000290*    DATUM:    26.11.2001                                       *
000300*****************************************************************
000310*    VERSION  1
000320****************************************************************
000330* AENDERUNGEN:                                                 *
000340* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
000350* -------------------------------------------------------------*
000360* ASL00103!26.11.01! 01 ! NEUERSTELLUNG             ! S. Walb  *
000370*         !        !    !                           !          *
000380*         !        !    !                           !          *
000390*                                                              *
000400* ENDE-LOGBUCH                                                 *
000410****************************************************************
000420****************************************************************
000430*    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
000440*    FÜR DIE FUNKTION DRVB0036                                 *
000450*    AUSSER: DEM FUNKTIONSNAMEN,                               *
000460*            DEM ERR-ERROR-BEREICH UND                         *
000470*            DEM STANDARD-STATUS-BEREICH                       *
000480*            (JEWEILS SEPARATE COPYS)                          *
000490*                                                              *
000500****************************************************************
000510*
000520*
000530 01 PI-DRVB0036.
000540    05 PI-DRVB0036-PROGRAMM           PIC  X(008).
000550    05 PI-DRVB0036-UFKT-CODE          PIC  9(002).
000560    05 PI-DRVB0036-ED-BESITZER        PIC  S9(009) COMP.
000570    05 PI-DRVB0036-DAT-VON            PIC  X(010).
000580    05 PI-DRVB0036-DAT-BIS            PIC  X(010).
000590*
000600 01 PO-DRVB0036.

000620    05 PO-DRVB0036-RC                 PIC  X(003).
000630       88 PO-DRVB0036-RC-OK                        VALUE 'OK '.
000640       88 PO-DRVB0036-RC-LEER                      VALUE 'LER'.
000650       88 PO-DRVB0036-RC-EOF                       VALUE 'EOF'.
000660       88 PO-DRVB0036-RC-NOT-FOUND                 VALUE 'NFD'.

000680    05 PO-DRVB0036-DATEN.
000690       10 PO-DRVB0036-ED-ST-NR         PIC  S9(009) COMP.
000700       10 PO-DRVB0036-ED-BETREUTER     PIC  S9(009) COMP.
000710*
000720***** ENDE COPY-MEMBER DRVB0036 *****************************
