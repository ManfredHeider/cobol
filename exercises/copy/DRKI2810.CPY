      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2810.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2810.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:38:28   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:27:58   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:16:50   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:52:46   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:43:28   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:07:18   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:18:42   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Nov 27 2013 08:19:33 T13EX77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2810                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2810                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.11.2013 / 08:19:33 / T13EX77
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2810
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2810.
           05 PI-DRKI2810-PROGRAMM                 PIC  X(008).
           05 PI-DRKI2810-UFKT-CODE                PIC  9(002).
              88 PO-DRKI2810-UFKT-FIRST                 VALUE 1.
              88 PO-DRKI2810-UFKT-NEXT                  VALUE 2.
              88 PO-DRKI2810-UFKT-QUIT                  VALUE 3.
              88 PO-DRKI2810-UFKT-UPDATE                VALUE 4.
 
      *
       01  PO-DRKI2810.
           05  PO-DRKI2810-RC                      PIC  X(003).
               88 PO-DRKI2810-RC-OK                     VALUE 'OK '.
               88 PO-DRKI2810-RC-EOF                    VALUE 'EOF'.
               88 PO-DRKI2810-RC-LEER                   VALUE 'LER'.
      *
           05  PO-DRKI2810-DATEN.
            10 'XX'-G68BVERTRAGSNR            PIC  X(0015).
            10 'XX'-G68BLFDNR                 PIC S9(4) COMP.
            10 'XX'-G68BPRODBEGINN            PIC  X(010).
            10 'XX'-G68BPRODUKTENDE           PIC  X(010).
            10 'XX'-NIL-G68BPRODUKTENDE       PIC S9(4) COMP.
            10 'XX'-G68BBETRAGANFANG          PIC S9(12)V9(2) COMP-3.
            10 'XX'-G68BFESTZINSENDE          PIC  X(010).
            10 'XX'-NIL-G68BFESTZINSENDE      PIC S9(4) COMP.
            10 'XX'-G68BBETRGAKTUELL          PIC S9(12)V9(2) COMP-3.
            10 'XX'-NIL-G68BBETRGAKTUELL      PIC S9(4) COMP.
            10 'XX'-G68BTILGBEGINN            PIC  X(010).
            10 'XX'-NIL-G68BTILGBEGINN        PIC S9(4) COMP.
            10 'XX'-G68BTILGUNGSART           PIC  X(0001).
            10 'XX'-G68BTILGUNGPROZ           PIC S9(2)V9(4) COMP-3.
            10 'XX'-NIL-G68BTILGUNGPROZ       PIC S9(4) COMP.
            10 'XX'-G68BNOMINALZINS           PIC S9(2)V9(4) COMP-3.
            10 'XX'-NIL-G68BNOMINALZINS       PIC S9(4) COMP.
            10 'XX'-G68BZAHLUNGSART           PIC  X(0001).
            10 'XX'-G68BANNUITAET             PIC S9(12)V9(2) COMP-3.
            10 'XX'-NIL-G68BANNUITAET         PIC S9(4) COMP.
            10 'XX'-G68BZUSAGEDATUM           PIC  X(010).
            10 'XX'-NIL-G68BZUSAGEDATUM       PIC S9(4) COMP.
            10 'XX'-G68BAUSZAHLDATUM          PIC  X(010).
            10 'XX'-NIL-G68BAUSZAHLDATUM      PIC S9(4) COMP.
            10 'XX'-G68BDARLEHENSART          PIC  X(0001).
            10 'XX'-G68BIBAN                  PIC  X(0034).
            10 'XX'-NIL-G68BIBAN              PIC S9(4) COMP.
            10 'XX'-G68BBIC                   PIC  X(0011).
            10 'XX'-NIL-G68BBIC               PIC S9(4) COMP.
            10 'XX'-G68BMANDATREF             PIC  X(0035).
            10 'XX'-NIL-G68BMANDATREF         PIC S9(4) COMP.
            10 'XX'-G68BABWEICHBZ             PIC  X(0001).
            10 'XX'-G68BNACHNAME              PIC  X(0030).
            10 'XX'-NIL-G68BNACHNAME          PIC S9(4) COMP.
            10 'XX'-G68BVORNAME               PIC  X(0030).
            10 'XX'-NIL-G68BVORNAME           PIC S9(4) COMP.
            10 'XX'-G68BGEBURTSDATUM          PIC  X(010).
            10 'XX'-NIL-G68BGEBURTSDATUM      PIC S9(4) COMP.
            10 'XX'-G68BANREDE                PIC  X(0001).
            10 'XX'-NIL-G68BANREDE            PIC S9(4) COMP.
            10 'XX'-G68BSTRASSE               PIC  X(0040).
            10 'XX'-NIL-G68BSTRASSE           PIC S9(4) COMP.
            10 'XX'-G68BPOSTLEITZAHL          PIC  X(0009).
            10 'XX'-NIL-G68BPOSTLEITZAHL      PIC S9(4) COMP.
            10 'XX'-G68BORT                   PIC  X(0030).
            10 'XX'-NIL-G68BORT               PIC S9(4) COMP.
            10 'XX'-G68BLAND                  PIC  X(0003).
            10 'XX'-NIL-G68BLAND              PIC S9(4) COMP.
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 
 
 

