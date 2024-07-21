      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2833.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2833.CPv  $
      *
      *   Rev 2.2   Mar 14 2014 10:24:14   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden - 
      *Migrationsprogramme
      *
      *   Rev 2.1   Jan 28 2014 09:20:50   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden-Mi
      *gration Tabelle Neukunde und Datum_Altkunde
      *
      *   Rev 2.0   Dec 18 2013 18:12:16   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                    *
      *     - ZWSP-Struktur der NEUKUNDE(TCKI0098)                     *
      *                                                                *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   132                                             *
      *--------------------------------------------------------------- *
      *LOGBUCH                                                         *
      *==========
      *        !        ! AENDERUNG                      !NAME         *
      *AUFTRAG !        !                                !             *
      *--------!--------!--------------------------------!-------------*
      *AKI01180!20.01.14! NEU                            !T. Flierl    *
      *--------!--------!--------------------------------!-------------*
      *        !        !                                !             *
      ******************************************************************

      *01  ZWSP-NEUKUNDE.
          05 'XX'-PERSONENID               PIC X(13).
          05 'XX'-LFDNR                    PIC S9(4) COMP.
          05 'XX'-KUNDENNUMMER             PIC S9(9) COMP.
          05 'XX'-VBNR                     PIC S9(9) COMP.
          05 'XX'-DIREKTION                PIC S9(4) COMP.
          05 'XX'-PARTGESELLSCHID          PIC S9(4) COMP.
          05 'XX'-VERTRAGSNUMMER           PIC X(15).
          05 'XX'-TAGESDATUM               PIC X(10).
          05 'XX'-KNDSTATUSIDALT           PIC X(01).
          05 'XX'-KNDSTATUSIDNEU           PIC X(01).
          05 'XX'-BWTUNGSKZDVAG            PIC X(02).
          05 'XX'-BWTUNGSKZVB              PIC X(02).
      *   05 'XX'-NIL-NKDBWTUNGSKZVB       PIC X(01).
          05 'XX'-KOMMISARISCHKZ           PIC X(01).
      *   05 'XX'-NIL-NKDKOMMISARISCHKZ    PIC X(01).
          05 'XX'-PGMNAME                  PIC X(08).
          05 'XX'-PRODUKT                  PIC X(10).
      *   05 'XX'-NIL-NKDPRODUKT           PIC X(01).
          05 'XX'-EREIGNISKZ               PIC S9(4) COMP.
          05 'XX'-LETZTEAENDERUNG          PIC X(26).
          05 'XX'-KUZUPERSONENID           PIC X(13).
      *   05 'XX'-NIL-NKDKUZUPERSONENID    PIC X(01).
      *
      ******************************************************************
      *          E n d e    Copy-Member DRKI2833                       *
      ******************************************************************

