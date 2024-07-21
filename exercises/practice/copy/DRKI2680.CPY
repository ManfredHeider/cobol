      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2680.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2680.CPv  $
      *
      *   Rev 2.1   Sep 21 2012 17:03:44   t13ex48
      *AKI01175: DeuBa-Anspracheschutz: Cobol-Sourcen RDTA
      *
      *   Rev 2.0   Sep 07 2012 18:08:30   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2680                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2680                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   Haendisch erstellt
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01175!20.09.12! 01 ! NEU                       ! HSE      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2680
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  CURSOR ueber Erledigt-KZ  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI2680.
           05  PI-DRKI2680-PROGRAMM            PIC  X(008).
           05  PI-DRKI2680-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI2680-DATEN.
             10 PI-DRKI2680-ERLKZ              PIC X(01).
      ***
       01 PO-DRKI2680.
           05 PO-DRKI2680-RC                    PIC  X(003).
             88 PO-DRKI2680-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2680-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2680-RC-EOF                       VALUE 'EOF'.

           05 PO-DRKI2680-DATEN.
             10 PO-DRKI2680-ASSVERTRAGSID    PIC X(13).
             10 PO-DRKI2680-ASSGESNR         PIC S9(4).
             10 PO-DRKI2680-ASSVNR           PIC X(15).
             10 PO-DRKI2680-ASSABSCHLVBNR    PIC S9(9).
             10 PO-DRKI2680-ASSERSTELLTTST   PIC X(26).
             10 PO-DRKI2680-ASSERLKZ         PIC X(01).
             10 PO-DRKI2680-ASSERLDAT        PIC X(10).
             10 PO-DRKI2680-LETZTEAENDERUNG  PIC X(26).
      *
      ******* ENDE COPY-MEMBER **************************************
