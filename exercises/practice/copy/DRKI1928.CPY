      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1928.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1928.CPv  $
      *
      *   Rev 2.1   Oct 18 2006 14:11:42   t13ex14
      *AKI00546: Migration OKI - Adressen + Leben + Unfall
      *
      *   Rev 2.0   Jun 28 2006 07:17:16   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME     *
      * -------------------------------------------------------------*
      * AKI00546|28.06.06|    | NEU                       |Peter Pohl*
      *         |        |    |                           |          *
      *         |        |    |                           |          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    Migration Adressen f}r OKI
      *    Vergabe der laufenden Nummer f}r OKI Leben_Person_Fremd
      *
      *    L{nge: 118 Byte
      *
      ****************************************************************

       01  DRKI1928.
           10 DRKI1928-VERTRAGSID              PIC  X(13).
           10 DRKI1928-BEZIEHUNGSARTKZ         PIC  X(01).
           10 DRKI1928-PERSONANZNAME           PIC  X(30).
           10 DRKI1928-PERSONANZNAME-NI        PIC  X(01).
           10 DRKI1928-PERSONANZVORNAME        PIC  X(30).
           10 DRKI1928-PERSONANZVORNAME-NI     PIC  X(01).
           10 DRKI1928-PERSONGEBGRUENDDAT      PIC  X(10).
           10 DRKI1928-PERSONGEBGRUEND-NI      PIC  X(01).
           10 DRKI1928-PERSABADRESSEID         PIC  X(13).
           10 DRKI1928-PERSABADRESSEID-NI      PIC  X(01).
           10 DRKI1928-PERSONANREDEID          PIC S9(4) COMP.
           10 DRKI1928-PERSONANREDEID-NI       PIC  X(01).
           10 DRKI1928-NATPHBPERSONENID        PIC  X(13).
           10 DRKI1928-NATPHBPERSONENID-NI     PIC  X(01).

      *-------------------- ENDE DRKI1928 ---------------------

