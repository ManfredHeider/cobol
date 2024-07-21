      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2815.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRKI2815.CPv  $
      *
      *   Rev 2.9   Mar 17 2015 14:39:24   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.8   Jan 13 2015 13:18:50   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.7   Jan 16 2014 14:42:58   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.6   Jan 16 2014 13:25:40   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.5   Jan 16 2014 13:08:18   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.4   Jan 15 2014 15:27:18   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.3   Jan 15 2014 13:04:36   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.2   Jan 15 2014 12:55:12   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.1   Jan 15 2014 08:04:02   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank Aktu
      *alisierung DB2
      *
      *   Rev 2.0   Nov 19 2013 15:18:16   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2815                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2815                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.11.2013 / 10:12:41 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2815
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
      *01  PI-DRKI2815.
 
      *    05  WU-BANKPRODUKT.
           05 'XX'-BKPVERTRAGSID           PIC  X(0013).
           05 'XX'-BKPLFDNR                PIC S9(4) COMP.
           05 'XX'-LETZTEAENDERUNG         PIC  X(026).
           05 'XX'-NIL-LETZTEAENDERUNG     PIC  X(01).
           05 'XX'-BKPPRODUKTID            PIC S9(4) COMP.
           05 'XX'-BKPKONTOART             PIC  X(0003).
           05 'XX'-NIL-BKPKONTOART         PIC  X(01).
           05 'XX'-BKPTELREVERSKZ          PIC  X(0001).
           05 'XX'-NIL-BKPTELREVERSKZ      PIC  X(01).
           05 'XX'-BKPONLREVERSKZ          PIC  X(0001).
           05 'XX'-NIL-BKPONLREVERSKZ      PIC  X(01).
           05 'XX'-BKPKREDITART            PIC  X(0006).
           05 'XX'-NIL-BKPKREDITART        PIC  X(01).
           05 'XX'-BKPDARLEHENSART         PIC  X(0001).
           05 'XX'-NIL-BKPDARLEHENSART     PIC  X(01).
           05 'XX'-BKPBILANZSCHLKTY        PIC S9(4) COMP.
           05 'XX'-BKPBILANZSCHLKNR        PIC S9(4) COMP.
           05 'XX'-BKPKONDSCHL             PIC  X(0002).
           05 'XX'-NIL-BKPKONDSCHL         PIC  X(01).
           05 'XX'-BKPZAHLWEISEKTY         PIC S9(4) COMP.
           05 'XX'-BKPZAHLWEISEKNR         PIC S9(4) COMP.
           05 'XX'-BKPDETAILKTY            PIC S9(4) COMP.
           05 'XX'-NIL-BKPDETAILKTY        PIC  X(01).
           05 'XX'-BKPDETAILKNR            PIC S9(15)V COMP-3.
           05 'XX'-NIL-BKPDETAILKNR        PIC  X(01).
           05 'XX'-BKPBLZKUNDEN            PIC  X(0008).
           05 'XX'-NIL-BKPBLZKUNDEN        PIC  X(01).
           05 'XX'-BKPBIC                  PIC  X(0011).
           05 'XX'-NIL-BKPBIC              PIC  X(01).
           05 'XX'-BKPIBAN                 PIC  X(0034).
           05 'XX'-NIL-BKPIBAN             PIC  X(01).
           05 'XX'-BKPABSCHLUSSVBNR        PIC S9(9) COMP.
           05 'XX'-NIL-BKPABSCHLUSSVBNR    PIC  X(01).
           05 'XX'-BKPEIGENVERMKZ          PIC  X(0001).
           05 'XX'-NIL-BKPEIGENVERMKZ      PIC  X(01).
           05 'XX'-BKPWAEHRSCHL            PIC S9(5)V COMP-3.
           05 'XX'-NIL-BKPWAEHRSCHL        PIC  X(01).
           05 'XX'-BKPDATEROFFNUNG         PIC  X(010).
           05 'XX'-NIL-BKPDATEROFFNUNG     PIC  X(01).
           05 'XX'-BKPDATSCHLIESS          PIC  X(010).
           05 'XX'-NIL-BKPDATSCHLIESS      PIC  X(01).
           05 'XX'-BKPDATKREDITZUS         PIC  X(010).
           05 'XX'-NIL-BKPDATKREDITZUS     PIC  X(01).
           05 'XX'-BKPDATKREDITAUSZ        PIC  X(010).
           05 'XX'-NIL-BKPDATKREDITAUSZ    PIC  X(01).
           05 'XX'-BKPENDEZINSFEST         PIC  X(010).
           05 'XX'-NIL-BKPENDEZINSFEST     PIC  X(01).
           05 'XX'-BKPVERTRAGSENDE         PIC  X(010).
           05 'XX'-NIL-BKPVERTRAGSENDE     PIC  X(01).
           05 'XX'-BKPDATFAELLIG           PIC  X(010).
           05 'XX'-NIL-BKPDATFAELLIG       PIC  X(01).
           05 'XX'-BKPEZRFREIJAHRE         PIC  X(010).
           05 'XX'-NIL-BKPEZRFREIJAHRE     PIC  X(01).
           05 'XX'-BKPDATBEGENDE           PIC  X(010).
           05 'XX'-NIL-BKPDATBEGENDE       PIC  X(01).
           05 'XX'-BKPDARLBETRAG           PIC S9(12)V9(2) COMP-3.
           05 'XX'-NIL-BKPDARLBETRAG       PIC  X(01).
           05 'XX'-BKPDARLBETRAKT          PIC S9(12)V9(2) COMP-3.
           05 'XX'-NIL-BKPDARLBETRAKT      PIC  X(01).
           05 'XX'-BKPSALDOBETR            PIC S9(12)V9(2) COMP-3.
           05 'XX'-NIL-BKPSALDOBETR        PIC  X(01).
           05 'XX'-BKPLIMITEXT             PIC S9(12)V9(2) COMP-3.
           05 'XX'-NIL-BKPLIMITEXT         PIC  X(01).
           05 'XX'-BKPSOLLZINS             PIC S9(2)V9(4) COMP-3.
           05 'XX'-NIL-BKPSOLLZINS         PIC  X(01).
           05 'XX'-BKPHABENZINS            PIC S9(2)V9(4) COMP-3.
           05 'XX'-NIL-BKPHABENZINS        PIC  X(01).
           05 'XX'-BKPUEBERZZINS           PIC S9(2)V9(4) COMP-3.
           05 'XX'-NIL-BKPUEBERZZINS       PIC  X(01).
           05 'XX'-BKPRATENHOEHE           PIC S9(12)V9(2) COMP-3.
           05 'XX'-NIL-BKPRATENHOEHE       PIC  X(01).
           05 'XX'-BKPANNUITAET            PIC S9(12)V9(2) COMP-3.
           05 'XX'-NIL-BKPANNUITAET        PIC  X(01).
           05 'XX'-BKPTILGUNGSART          PIC  X(0001).
           05 'XX'-NIL-BKPTILGUNGSART      PIC  X(01).
           05 'XX'-BKPTILGUNGPROZ          PIC S9(2)V9(4) COMP-3.
           05 'XX'-NIL-BKPTILGUNGPROZ      PIC  X(01).
           05 'XX'-BKPZINSANTEIL           PIC S9(12)V9(2) COMP-3.
           05 'XX'-NIL-BKPZINSANTEIL       PIC  X(01).
           05 'XX'-BKPTILGUNGANF           PIC S9(12)V9(2) COMP-3.
           05 'XX'-NIL-BKPTILGUNGANF       PIC  X(01).
 
 

