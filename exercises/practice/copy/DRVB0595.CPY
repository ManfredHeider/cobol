000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   DRVB0595.CPY  $
000110*    $Revision:   2.2  $
000120*
000130*    $Log:   L:/mf/cpy/DRVB0595.CPv  $
000140*
000150*   Rev 2.2   Nov 05 2002 09:57:36   t13ae27
000160*Längenänderung Email/URL auf 83 
000170*
000180*   Rev 2.0   Feb 25 2002 17:33:40   p13sp20
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220*
000230*    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
000240*    FÜR DIE FUNKTION DRVB0595
000250*    AUSSER: DEM FUNKTIONSNAMEN,
000260*            DEM ERR-ERROR-BEREICH UND
000270*            DEM STANDARD-STATUS-BEREICH
000280*            (JEWEILS SEPARATE COPYS)
000290*
000300*    >>>  SELECT ...                <<<
000310*
000320****************************************************************
000330*
000340 01  PI-DRVB0595.
000350     05  PI-DRVB0595-MA-ST-NR           PIC S9(009) COMP.
000360     05  PI-DRVB0595-KZ-EMAIL-URL       PIC X(001).
000370*
000380 01  PO-DRVB0595.
000390     05  PO-DRVB0595-RC                 PIC  X(003).
000400         88 PO-DRVB0595-RC-OK                  VALUE 'OK '.
000410         88 PO-DRVB0595-RC-NOK                 VALUE 'NOK'.
000420         88 PO-DRVB0595-RC-NOT-FOUND           VALUE 'NFD'.
000430         88 PO-DRVB0595-RC-ERR                 VALUE 'ERR'.
000440*
000450*    05  PO-DRVB0595-EMAIL-URL          PIC X(070).
000460     05  PO-DRVB0595-EMAIL-URL          PIC X(083).
000470     05  PO-DRVB0595-DAT-BIS            PIC X(010).
000480     05  PO-DRVB0595-SST-KZ             PIC X(001).
000490*
000500*********************** ENDE COPY-MEMBER *************************
