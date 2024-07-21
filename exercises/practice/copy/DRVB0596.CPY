000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   DRVB0596.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/DRVB0596.CPv  $
000140*
000150*   Rev 2.1   Sep 05 2002 13:32:20   t13ae30
000160*AVB00627 - ANPASSUNG SST WG. SeminarIS
000170*
000180*   Rev 2.0   Feb 25 2002 17:33:46   p13sp20
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220*    COPY-MEMBER: DRVB0596                                      *
000230*    --------------------------------------------------------   *
000240*    SCHNITTSTELLE FÜR DAS UPRO ZWVB0019                        *
000250*    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
000260*    --------------------------------------------------------   *
000270*    ERSTELLT: WILLIG                                           *
000280*    DATUM:    02.09.2002                                       *
000290*****************************************************************
000300*    VERSION  1
000310****************************************************************
000320* AENDERUNGEN:                                                 *
000330* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
000340* -------------------------------------------------------------*
000350* AVB00627!30.08.02! 01 ! NEUERSTELLUNG             ! WILLIG   *
000360*         !        !    !                           !          *
000370*         !        !    !                           !          *
000380*                                                              *
000390* ENDE-LOGBUCH                                                 *
000400****************************************************************
000410****************************************************************
000420*    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
000430*    FÜR DIE FUNKTION DRVB0596                                 *
000440*    AUSSER: DEM FUNKTIONSNAMEN,                               *
000450*            DEM ERR-ERROR-BEREICH UND                         *
000460*            DEM STANDARD-STATUS-BEREICH                       *
000470*            (JEWEILS SEPARATE COPYS)                          *
000480*                                                              *
000490****************************************************************
000500*
000510*
000520 01 PI-DRVB0596.
000530    05 PI-DRVB0596-DIR-NR             PIC S9(004) COMP.
000540    05 PI-DRVB0596-DL-BEZUGS-NR       PIC S9(009) COMP.
000550*
000560 01 PO-DRVB0596.
000570    05 PO-DRVB0596-RC                 PIC  X(003).
000580       88 PO-DRVB0596-RC-OK                        VALUE 'OK '.
000590       88 PO-DRVB0596-RC-LEER                      VALUE 'LER'.
000600       88 PO-DRVB0596-RC-EOF                       VALUE 'EOF'.
000610       88 PO-DRVB0596-RC-NOT-FOUND                 VALUE 'NFD'.

000630*
000640***** Ende Copy-Member DRVB0596 *****************************
