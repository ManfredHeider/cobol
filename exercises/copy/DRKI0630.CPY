      * P13SP99   Stand: 06/12/98    18:02:33
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0630.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0630.CPv  $
      *
      *   Rev 2.3   Sep 18 2012 13:57:00   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.2   Feb 24 2003 08:55:32   t13ex27
      *AKI00303 - Kommisarische Kundenübertragung
      *
      *   Rev 2.1   Nov 15 2002 15:37:20   t13ex58
      *AKI00305: Vertragsid eingefügt
      *
      *   Rev 2.0   Apr 16 1999 13:19:18   T13EX19
      *aki00028
      *
      *   Rev 1.0   25 Aug 1998 09:25:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0630 FUNKTION:VERTRAGSDATEN ERMITTELN =  *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:                                                    *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00006!17.09.98! 01 !NEU                        !BRENNECKE *
      * AKI00305!21.10.02! 02 !Feld Vertragsid eingefügt  !W.Morfeld *
      * AKI00303!28.11.02! 02 !Feld VRBKOMMBETRKZeingefügt!W.Morfeld *
      * AKI00886!25.01.12!2.3 ! Neue Kundenaktualisierung !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0630                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: VERTRAGSDATENERMITTELN LIST                     *
      *              - ALLE VERTRAEGE EINES VBS ERMITTELN            *
      ****************************************************************
       01 PI-DRKI0630.
           05  PI-DRKI0630-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI0630-DATEN.
             10  PI-DRKI0630-DIRAENDPERSONENID    PIC  X(013).
             10  PI-DRKI0630-DIRAENDPERSROLLE     PIC  X(001).

      ***
       01 PO-DRKI0630.
          05 PO-DRKI0630-RC                     PIC  X(003).
             88 PO-DRKI0630-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0630-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0630-RC-EOF                       VALUE 'EOF'.
             88 PO-DRKI0630-RC-FP                        VALUE 'FP '.
          05 PO-DRKI0630-DATEN.
             10 PO-DRKI0630-VERVERTRAGSNUMMER     PIC  X(015).
             10 PO-DRKI0630-VERPARTGESELLSCHID    PIC  9(004).
             10 PO-DRKI0630-VERPRUEFZIFFVNR       PIC  9(002).
             10 PO-DRKI0630-VERINTERNEVERTRNR     PIC  9(007).
021021       10 PO-DRKI0630-VERVERTRAGSID         PIC  X(013).
021128       10 PO-DRKI0630-VRBKOMMBETRKZ         PIC  X(001).
             10 PO-DRKI0630-VRBGUELTIGKEITSDAT    PIC  X(010).
             10 PO-DRKI0630-VRBABLAUFDATUM        PIC  X(010).
             10 PO-DRKI0630-VRBPABADRESSEID       PIC  X(013).
      *
      ******** Ende Copy-Member **************************************
