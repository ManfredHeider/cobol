      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0013.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0013.CPv  $
      *
      *   Rev 2.1   Mar 30 2010 09:49:52   t13ex27
      *AKI00667: Bereinigung der KI-DB in allen Umgebungen-Tabelle 
      *INTERNETADRESSEN
      *
      *   Rev 2.0   Mar 29 2010 12:31:36   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-TCKI0013.
           10 IADID                       PIC  X(13).
           10 IADANREDE                   PIC  X(10).
           10 IADNACHNAME                 PIC  X(30).
           10 IADVORNAME                  PIC  X(30).
           10 IADLKZ                      PIC  X(3).
           10 IADPLZ                      PIC  X(10).
           10 IADORT                      PIC  X(30).
           10 IADSTR                      PIC  X(45).
           10 IADTELEFON                  PIC  X(25).
           10 IADTELEFAX                  PIC  X(25).
           10 IADEMAIL                    PIC  X(70).
           10 IADINFOKZ1                  PIC  X(1).
           10 IADINFOKZ2                  PIC  X(1).
           10 IADINFOKZ3                  PIC  X(1).
           10 IADINFOKZ4                  PIC  X(1).
           10 IADINFOKZ5                  PIC  X(1).
           10 IADINFOKZ6                  PIC  X(1).
           10 IADINFOKZ7                  PIC  X(1).
           10 IADINFOKZ8                  PIC  X(1).
           10 IADINFOKZ9                  PIC  X(1).
           10 IADINFOKZ10                 PIC  X(1).
           10 IADVBSUCHEAB8               PIC  S9(9) COMP.
           10 IADVBSUCHEAB9               PIC  S9(9) COMP.
           10 IADVBSUCHEAB10              PIC  S9(9) COMP.
           10 IADTEXT8                    PIC  X(20).
           10 IADTEXT9                    PIC  X(20).
           10 IADTEXT10                   PIC  X(20).
           10 IADMAXSTUFE                 PIC  S9(9) COMP.
           10 IADERREICHBAR               PIC  X(25).
           10 IADZUGVBID                  PIC  X(13).
           10 NIL-IADZUGVBID              PIC X(01).
           10 IADPOSTEKZ                  PIC  X(1).
           10 IADBGAM                     PIC  X(010).
           10 IADBGAUSGABE                PIC  X(10).
           10 IADGEWSPANT                 PIC  X(50).
           10 IADKUNDENID                 PIC  X(13).
           10 NIL-IADKUNDENID             PIC X(01).
           10 IADKUNDENSTATUSID           PIC  X(1).
           10 LETZTEAENDERUNG             PIC  X(026).
           10 IADINDIVTEXT.
             49 IADINDIVTEXT-LEN            PIC S9(4) COMP.
             49 IADINDIVTEXT-TEXT            PIC X(16000).
           10 IADANFMANDANT               PIC  S9(9) COMP.
           10 NIL-IADANFMANDANT           PIC X(01).
           10 IADZIELMANDANT              PIC  S9(9) COMP.
           10 NIL-IADZIELMANDANT          PIC X(01).
           10 IADMITARBEITERNR            PIC  S9(9) COMP.
           10 NIL-IADMITARBEITERNR        PIC X(01).
           10 IADSBMITARBEITNR            PIC  S9(9) COMP.
           10 NIL-IADSBMITARBEITNR        PIC X(01).
           10 IADKUNDE-NUMMER             PIC  S9(19)V COMP-3.
           10 NIL-IADKUNDE-NUMMER         PIC X(01).
