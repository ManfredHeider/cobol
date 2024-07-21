000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   DRVB0587.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/DRVB0587.CPv  $
000140*
000150*   Rev 2.1   Jun 20 2002 14:24:58   t13ae13
000160*AVB00606: Einführung Seminare für DVAG-Wien
000170*
000180*   Rev 2.0   Feb 25 2002 17:32:52   p13sp20
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220***************************************************************
000230* AENDERUNGEN:                                                 *
000240* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
000250* -------------------------------------------------------------*
000260* AVB00606!13.06.02!2.1 ! NEUERSTELLUNG             ! Walb     *
000270*         !        !    !                           !          *
000280*         !        !    !                           !          *
000290*         !        !    !                           !          *
000300*                                                              *
000310* ENDE-LOGBUCH                                                 *
000320****************************************************************
000330*                                                              *
000340*    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
000350*    FÜR DIE FUNKTION DRVB0587                                 *
000360*    AUSSER: DEM FUNKTIONSNAMEN,                               *
000370*            DEM ERR-ERROR-BEREICH UND                         *
000380*            DEM STANDARD-STATUS-BEREICH                       *
000390*            (JEWEILS SEPARATE COPYS)                          *
000400*                                                              *
000410****************************************************************
000420 01 PI-DRVB0587.
000430    05 PI-DRVB0587-DATEN.
000440       10 PI-DRVB0587-MA-ST-NR         PIC S9(009) COMP.
000450*
000460 01 PO-DRVB0587.
000470    05 PO-DRVB0587-RC                  PIC  X(003).
000480       88 PO-DRVB0587-RC-OK            VALUE 'OK '.
000490       88 PO-DRVB0587-RC-NFD           VALUE 'NFD'.
000500    05 PO-DRVB0587-DATEN.
000510       10 PO-DRVB0587-MA-FG-DAT        PIC  X(010).
000520       10 PO-DRVB0587-MA-127-DAT       PIC  X(010).
000530******** ENDE COPY-MEMBER **************************************
