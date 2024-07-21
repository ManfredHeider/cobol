      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1226.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1226.CPv  $
      *
      *   Rev 2.2   Oct 08 2009 17:48:24   t13ex48
      *AKI00890: RDTA-Komplettbestand mandantenfähig machen
      *
      *   Rev 2.1   Feb 21 2003 11:36:36   t13ex27
      *FKI00709 - Neuer Cursor DRKI1226
      *
      *   Rev 2.0   22 Mar 2001 15:50:40   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1226 FUNKTION:Vertragsdaten für AML      *
      *                                   ermitteln                  *
      *                                   LIST                       *
      *    -------------------------------------------------------   *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00709!21.02.03!2.1 !NEU                        !IFR       *
      *---------!--------!----!---------------------------!----------*
      * AKI00890!08.10.09!    !Zusaetzliches Feld MANDANT ! HSE      *
      *---------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1226                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION:                                                 *
      *                                                              *
      ****************************************************************
       01 PI-DRKI1226.
           05  PI-DRKI1226-PROGRAMM             PIC  X(008).
           05  PI-DRKI1226-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1226-DATEN.
             10  PI-DRKI1226-GES-VNR-VON        PIC  X(013).
             10  PI-DRKI1226-GES-VNR-BIS        PIC  X(013).
      ***
       01 PO-DRKI1226.
          05 PO-DRKI1226-RC                     PIC  X(003).
             88 PO-DRKI1226-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1226-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1226-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI1226-DATEN.
             10 PO-DRKI1226-VERINTERNEVERTNR     PIC S9(09) COMP.
             10 PO-DRKI1226-ADMADIREKTION        PIC S9(04) COMP.
             10 PO-DRKI1226-VERVERTRAGSNUMMER    PIC X(15).
             10 PO-DRKI1226-MITARBEITERNR        PIC S9(09) COMP.
             10 PO-DRKI1226-VERPRUEFZIFFVNR      PIC S9(04) COMP.
             10 PO-DRKI1226-VERVERTRZUSTANDID    PIC S9(04) COMP.
             10 PO-DRKI1226-ADMAPERSONENID       PIC X(13).
             10 PO-DRKI1226-VERVERTRAGSID        PIC X(13).
             10 PO-DRKI1226-VRBKOMMBETRKZ        PIC X(01).
             10 PO-DRKI1226-LTSTARBEZEICHNUNG    PIC X(15).
081009       10 PO-DRKI1226-VERMANDANTNR         PIC S9(09) COMP.
      *
      ******** Ende Copy-Member **************************************
