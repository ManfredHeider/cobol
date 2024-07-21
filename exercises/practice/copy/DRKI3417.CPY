      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3417.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3417.CPv  $
      *
      *   Rev 2.2   Nov 28 2018 17:30:32   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Sep 11 2018 12:16:42   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 6 2018 08:38:27 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3417                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3417                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.09.2018 / 15:41:33 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI3417
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  NEXT VALUE<<<
      *
      ****************************************************************
      *
       01  PI-DRKI3417.
           05  PI-DRKI3417-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3417-SPSPARTGESELLID   PIC S9(4) COMP.
             10 PI-DRKI3417-SPSVERTRAGSNR   PIC  X(0015).
END   *
 
 
           05  PI-DRKI3417-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3417-SPSAKTION-KZN   PIC  X(0001).
             10 PI-DRKI3417-SPSERSTELLTTST   PIC  X(0026).
             10 PI-DRKI3417-SPSVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI3417.
           05  PO-DRKI3417-RC                     PIC  X(003).
               88 PO-DRKI3417-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3417-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
           05  PO-DRKI3417-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3417-SPSLFDNR   PIC S9(4) COMP.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER **************************************
 

