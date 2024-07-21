      *****************************************************************
      *    COPY-MEMBER: DRVB1321                                      * 
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0041                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    24.09.2021                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * VLA-3405   !24.09.21! NEU                          ! KLEEMEIER*
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1321 - ERMITTELN VB-DATEN FÜR BRIEFERSTELLUNG * 
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1321.
           05  PI-DRVB1321-DATEN.
               10  PI-DRVB1321-MA-ST-NR          PIC S9(009) BINARY.
               10  PI-DRVB1321-ZUSTELL           PIC  X(001).
      *
       01  PO-DRVB1321.
           05  PO-DRVB1321-RC                    PIC  X(003).
               88  PO-DRVB1321-RC-OK                  VALUE 'OK '.
               88  PO-DRVB1321-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB1321-DATEN.
               10  PO-DRVB1321-PZ                PIC  X(001).
               10  PO-DRVB1321-ST-BEZ-KURZ       PIC  X(004).
               10  PO-DRVB1321-KNR-ANREDE        PIC S9(004) BINARY.
               10  PO-DRVB1321-MAN-TITEL-ADEL    PIC  X(010).
               10  PO-DRVB1321-MAN-TITEL-AKAD    PIC  X(010).
               10  PO-DRVB1321-MAN-NAME-GK       PIC  X(040).
               10  PO-DRVB1321-MAN-VORNAME-GK    PIC  X(020).
               10  PO-DRVB1321-MAAN-STRASSE-GK   PIC  X(040).
               10  PO-DRVB1321-MAAN-PLZ-AKTUELL  PIC  X(009).
               10  PO-DRVB1321-MAAN-ORT-GK       PIC  X(040).
               10  PO-DRVB1321-MAAN-POSTFACH     PIC  X(006).
               10  PO-DRVB1321-MAAN-PLZ-PF       PIC  X(009).
               10  PO-DRVB1321-MAAN-ORT-PF       PIC  X(040).
               10  PO-DRVB1321-MAAN-WOHNT-BEI    PIC  X(040).
               10  PO-DRVB1321-KNR-GRND          PIC S9(004) BINARY.
               10  PO-DRVB1321-AUSL-BEZ-KURZ     PIC  X(004). 
               10  PO-DRVB1321-DIR-NR            PIC S9(004) BINARY. 
               10  PO-DRVB1321-KNR-GESELL-TYP    PIC S9(004) BINARY.
      ****
      **** ENDE COPY DRVB1321 ****************************************
      ****