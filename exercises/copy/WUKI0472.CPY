      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09/10/14
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0472.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/WUKI0472.CPv  $
      *
      *   Rev 2.6   Mar 17 2015 14:38:54   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.5   Mar 11 2015 12:29:18   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 09 2015 11:18:04   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Mar 02 2015 10:54:04   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Feb 25 2015 11:44:48   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jan 13 2015 13:09:10   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Oct 9 2014 12:56:28   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-G68_BAUFI.
           10 G68BVERTRAGSNR                PIC  X(0015).
           10 G68BLFDNR                     PIC S9(4) COMP.
           10 G68BPRODBEGINN                PIC  X(010).
           10 G68BPRODUKTENDE               PIC  X(010).
           10 NIL-G68BPRODUKTENDE           PIC  X(01).
           10 G68BBETRAGANFANG              PIC S9(12)V9(2) COMP-3.
           10 G68BFESTZINSENDE              PIC  X(010).
           10 NIL-G68BFESTZINSENDE          PIC  X(01).
           10 G68BBETRGAKTUELL              PIC S9(12)V9(2) COMP-3.
           10 NIL-G68BBETRGAKTUELL          PIC  X(01).
           10 G68BTILGBEGINN                PIC  X(010).
           10 NIL-G68BTILGBEGINN            PIC  X(01).
           10 G68BTILGUNGSART               PIC  X(0001).
           10 G68BTILGUNGPROZ               PIC S9(2)V9(4) COMP-3.
           10 NIL-G68BTILGUNGPROZ           PIC  X(01).
           10 G68BNOMINALZINS               PIC S9(2)V9(4) COMP-3.
           10 NIL-G68BNOMINALZINS           PIC  X(01).
           10 G68BZAHLUNGSART               PIC  X(0001).
           10 G68BANNUITAET                 PIC S9(12)V9(2) COMP-3.
           10 NIL-G68BANNUITAET             PIC  X(01).
           10 G68BZUSAGEDATUM               PIC  X(010).
           10 NIL-G68BZUSAGEDATUM           PIC  X(01).
           10 G68BAUSZAHLDATUM              PIC  X(010).
           10 NIL-G68BAUSZAHLDATUM          PIC  X(01).
           10 G68BDARLEHENSART              PIC  X(0001).
           10 G68BIBAN                      PIC  X(0034).
           10 NIL-G68BIBAN                  PIC  X(01).
           10 G68BBIC                       PIC  X(0011).
           10 NIL-G68BBIC                   PIC  X(01).
           10 G68BMANDATREF                 PIC  X(0035).
           10 NIL-G68BMANDATREF             PIC  X(01).
           10 G68BABWEICHBZ                 PIC  X(0001).
           10 G68BNACHNAME                  PIC  X(0030).
           10 NIL-G68BNACHNAME              PIC  X(01).
           10 G68BVORNAME                   PIC  X(0030).
           10 NIL-G68BVORNAME               PIC  X(01).
           10 G68BGEBURTSDATUM              PIC  X(010).
           10 NIL-G68BGEBURTSDATUM          PIC  X(01).
           10 G68BANREDE                    PIC  X(0001).
           10 NIL-G68BANREDE                PIC  X(01).
           10 G68BSTRASSE                   PIC  X(0040).
           10 NIL-G68BSTRASSE               PIC  X(01).
           10 G68BPOSTLEITZAHL              PIC  X(0009).
           10 NIL-G68BPOSTLEITZAHL          PIC  X(01).
           10 G68BORT                       PIC  X(0030).
           10 NIL-G68BORT                   PIC  X(01).
           10 G68BLAND                      PIC  X(0003).
           10 NIL-G68BLAND                  PIC  X(01).
           10 ZUGANG                        PIC  X(026).
           10 VON                           PIC  X(026).
           10 BIS                           PIC  X(026).
 
 
 
 
 

