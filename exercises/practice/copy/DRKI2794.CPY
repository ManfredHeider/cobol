      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2794.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2794.CPv  $
      *
      *   Rev 2.7   Mar 17 2015 14:37:16   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.6   Mar 11 2015 12:26:46   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.5   Mar 09 2015 11:15:42   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 02 2015 10:51:36   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Feb 25 2015 11:42:02   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Jan 13 2015 13:05:42   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jul 24 2014 15:16:46   t13ex77
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Nov 27 2013 08:19:33 T13EX77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2794                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2794                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         09.10.2014 / 12:45:49 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2794
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2794.
           05  PI-DRKI2794-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2794-G68VVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI2794.
           05  PO-DRKI2794-RC                     PIC  X(003).
               88 PO-DRKI2794-RC-OK               VALUE 'OK '.
               88 PO-DRKI2794-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2794-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2794-G68VABSCHLVBNR   PIC S9(9) COMP.
             10 PO-DRKI2794-G68VDSSPERRDATUM   PIC  X(010).
             10 PO-DRKI2794-G68VDSSPERRE   PIC  X(0001).
             10 PO-DRKI2794-G68VMANDANT   PIC S9(9) COMP.
             10 PO-DRKI2794-G68VSTATUS   PIC  X(0001).
             10 PO-DRKI2794-G68VVRTRGBEGINN   PIC  X(010).
             10 PO-DRKI2794-G68VVRTRGENDE   PIC  X(010).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 
 
 

