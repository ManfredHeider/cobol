000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   DRVB0592.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/DRVB0592.CPv  $
000140*
000150*   Rev 2.1   Jul 24 2002 10:26:18   t13ae35
000160*FSL00099 - neu, KL zum Zeitpunkt, Funktion zu ZXVB0040
000170*
000180*   Rev 2.0   Feb 25 2002 17:33:22   p13sp20
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220*    COPY-MEMBER: DRVB0592                                      *
000230*    --------------------------------------------------------   *
000240*    SCHNITTSTELLE FÜR DAS UPRO ZXVB0040                        *
000250*    zu KL-Typ und Stichtag werden alle KL's (VB-Nr)            *
000260*    zurück geliefert                                           *
000270*****************************************************************
000280* AENDERUNGEN:                                                 *
000290* AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
000300* -------------------------------------------------------------*
000310* FSL00099!23.07.02! 01 ! NEU                       ! J.Hering *
000320*         !        !    !                           !          *
000330*                                                              *
000340* ENDE-LOGBUCH                                                 *
000350****************************************************************
000360*    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
000370*    FÜR DIE FUNKTION DRVB0592                                 *
000380*    AUSSER: DEM FUNKTIONSNAMEN,                               *
000390*            DEM ERR-ERROR-BEREICH UND                         *
000400*            DEM STANDARD-STATUS-BEREICH                       *
000410*            (JEWEILS SEPARATE COPYS)                          *
000420*                                                              *
000430****************************************************************
000440*
000450 01 PI-DRVB0592.
000460    05 PI-DRVB0592-UFKT-CODE          PIC   9(002).
000470    05 PI-DRVB0592-KNR-KL-TYP         PIC  S9(004) COMP.
000480    05 PI-DRVB0592-KTY-KL-TYP         PIC  S9(004) COMP.
000490    05 PI-DRVB0592-ZEITPUNKT          PIC   X(010).
000500*
000510*------Rückgabewerte        -----------------------------------*
000520 01 PO-DRVB0592.

000540    05 PO-DRVB0592-RC                 PIC   X(003).
000550       88 PO-DRVB0592-RC-OK                        VALUE 'OK '.
000560       88 PO-DRVB0592-RC-LEER                      VALUE 'LER'.
000570       88 PO-DRVB0592-RC-EOF                       VALUE 'EOF'.
000580       88 PO-DRVB0592-RC-NOT-FOUND                 VALUE 'NFD'.

000600    05 PO-DRVB0592-DATEN.
000610       10 PO-DRVB0592-KL-ST-NR        PIC  S9(009)  COMP.
000620*

