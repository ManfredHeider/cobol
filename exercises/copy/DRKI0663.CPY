      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0663.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0663.CPv  $
      *
      *   Rev 2.5   Oct 08 2009 17:48:18   t13ex48
      *AKI00890: RDTA-Komplettbestand mandantenfähig machen
      *
      *   Rev 2.4   Jan 21 2003 16:26:52   t13ex27
      *AKI00303 - Kommissarische Kundenübertragung
      *
      *   Rev 2.3   Nov 15 2002 15:37:50   t13ex58
      *AKI00305: Vertragsid eingefügt
      *
      *   Rev 2.2   Mar 20 2000 09:25:46   T13EX48
      *FKI00348
      *Vertragszustand bei DIT berücksichtigen
      *
      *   Rev 2.1   Dec 07 1999 14:41:00   T13EX39
      *AKI00066 Rück-DTA
      *
      *   Rev 2.0   Feb 27 1999 12:58:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   25 Aug 1998 09:28:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0663 FUNKTION:VERTRAGSDATEN ERMITTELN    *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    15.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00066!30.11.99! 01 !NEU                        !WHO       *
      *---------!--------!----!---------------------------!----------*
      * FKI00348!16.03.00!    !Erweiterung PO-Bereich     ! HSE      *
      *---------!--------!----!---------------------------!----------*
      * AKI00274!10.06.02!    !Zusaetzliches Feld f. DB   ! HSE      *
      *         !        !    !- ADMAPERSONEID            !          *
      *---------!--------!----!---------------------------!----------*
      * AKI00305!04.11.02!    !Zusaetzliches Feld f. AML  ! MORFELD  *
      *         !        !    !- VERVERTRAGSID            !          *
      *---------!--------!----!---------------------------!----------*
      * AKI00303!04.12.02!    !Zusaetzliches Feld f. AML  ! MORFELD  *
      *         !        !    !- VRBKOMMBETRKZ            !          *
      *---------!--------!----!---------------------------!----------*
      * AKI00890!08.10.09!    !Zusaetzliches Feld MANDANT ! HSE      *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0663                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: Alle Vertraege eines Produktgebers              *
      *              zur vorgegebenen Gesellschfts-ID ermitteln      *
      *
      ****************************************************************
       01 PI-DRKI0663.
           05  PI-DRKI0663-PROGRAMM             PIC  X(008).
           05  PI-DRKI0663-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI0663-DATEN.
             10  PI-DRKI0663-GES-VNR-VON        PIC  X(013).
             10  PI-DRKI0663-GES-VNR-BIS        PIC  X(013).

      ***
       01 PO-DRKI0663.
          05 PO-DRKI0663-RC                     PIC  X(003).
             88 PO-DRKI0663-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0663-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI0663-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI0663-DATEN.
             10 PO-DRKI0663-VERINTERNEVERTNR     PIC S9(09) COMP.
             10 PO-DRKI0663-ADMADIREKTION        PIC S9(04) COMP.
             10 PO-DRKI0663-VERVERTRAGSNUMMER    PIC X(15).
             10 PO-DRKI0663-MITARBEITERNR        PIC S9(09) COMP.
             10 PO-DRKI0663-VERPRUEFZIFFVNR      PIC S9(4) COMP.
160300       10 PO-DRKI0663-VERVERTRZUSTANDID    PIC S9(4) COMP.
100602       10 PO-DRKI0663-ADMAPERSONENID       PIC X(13).
041102       10 PO-DRKI0663-VERVERTRAGSID        PIC X(13).
041202       10 PO-DRKI0663-VRBKOMMBETRKZ        PIC X(01).
081009       10 PO-DRKI0663-VERMANDANTNR         PIC S9(09) COMP.

      *
      ******** Ende Copy-Member **************************************

