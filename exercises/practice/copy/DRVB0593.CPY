000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   DRVB0593.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/DRVB0593.CPv  $
000140*
000150*   Rev 2.1   Jul 24 2002 10:26:54   t13ae35
000160*FSL00099 - neu, RFT zum Zeitpunkt, Funktion für ZXVB0005
000170*
000180*   Rev 2.0   Feb 25 2002 17:33:28   p13sp20
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220****************************************************************
000230* AENDERUNGEN:                                                 *
000240* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
000250* -------------------------------------------------------------*
000260* FSL00099!23.07.02! 01 ! NEU                       ! J.Hering *
000270*         !        !    !                           !          *
000280*                                                              *
000290* ENDE-LOGBUCH                                                 *
000300****************************************************************
000310****************************************************************
000320*    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
000330*    FÜR DIE FUNKTION DRVB0593                                 *
000340*    AUSSER: DEM FUNKTIONSNAMEN,                               *
000350*            DEM ERR-ERROR-BEREICH UND                         *
000360*            DEM STANDARD-STATUS-BEREICH                       *
000370*            (JEWEILS SEPARATE COPYS)                          *
000380*                                                              *
000390****************************************************************
000400*
000410 01 PI-DRVB0593.
000420    05 PI-DRVB0593-UFKT-CODE          PIC  9(002).
000430    05 PI-DRVB0593-ZEITPUNKT          PIC   X(010).
000440*
000450 01 PO-DRVB0593.

000470    05 PO-DRVB0593-RC                 PIC  X(003).
000480       88 PO-DRVB0593-RC-OK                        VALUE 'OK '.
000490       88 PO-DRVB0593-RC-LEER                      VALUE 'LER'.
000500       88 PO-DRVB0593-RC-EOF                       VALUE 'EOF'.
000510       88 PO-DRVB0593-RC-NOT-FOUND                 VALUE 'NFD'.

000530    05 PO-DRVB0593-DATEN.
000540       10 PO-DRVB0593-RFT-ST-NR       PIC S9(009)  COMP.
000550       10 PO-DRVB0593-RFT-DAT-VON     PIC X(010).
000560       10 PO-DRVB0593-RFT-DAT-BIS     PIC X(010).
000570*

