      * T13AE38   Stand: 30/08/98    13:47:20
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0361.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0361.CPv  $
      *
      *   Rev 2.1   Mar 25 1999 09:20:40   T13AE38
      *AKI00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:02:14   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI000001!        ! 01 ! NEUERSTELLUNG             ! S.JOST   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0361                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0361.
          05 PI-DRKI0361-PZVP-VERSION             PIC S9(004) BINARY.
          05 PI-DRKI0361-PROGRAMM                 PIC  X(008).
          05 PI-DRKI0361-UFKT                     PIC  9(002).
          05 PI-DRKI0361-SCHALTER                 PIC  9.
          05 PI-DRKI0361-DATEN.
            07 PI-DRKI0361-AML-PERSROLLE   PIC 9(4).
               88 PI-DRKI0361-AML-K1                      VALUE 0001.
               88 PI-DRKI0361-AML-K2                      VALUE 0002.
               88 PI-DRKI0361-AML-K3                      VALUE 0003.
               88 PI-DRKI0361-AML-K4                      VALUE 0004.
               88 PI-DRKI0361-AML-K5                      VALUE 0005.
               88 PI-DRKI0361-AML-K29                     VALUE 0029.
      *
       01 PO-DRKI0361.
          05 PO-DRKI0361-RC                       PIC  X(003).
             88 PO-DRKI0361-RC-OK                        VALUE 'OK '.
             88 PO-DRKI0361-RC-NOK                       VALUE 'NOK'.
          05 PO-DRKI0361-DATEN.
           10  PO-DRKI0361-VRBPERSONENROLLEKZ   PIC X.
           10  PO-DRKI0361-S-VRBPKZ-GEFUELLT    PIC X.
           10  PO-DRKI0361-VTRPERSONENROLLEKZ   PIC X.
           10  PO-DRKI0361-S-VTRPKZ-GEFUELLT    PIC X.
           10  PO-DRKI0361-VTRBEZIEHUNGSARTKZ   PIC X.
           10  PO-DRKI0361-S-VTRBEZ-GEFUELLT    PIC X.
      *
      ***** ENDE-COPY-MEMBER *****************************************
